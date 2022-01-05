/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import cats.data.EitherT
import models.DepartureAlreadyLocked
import models.DepartureId
import models.ErrorState
import models.FailedToLock
import models.FailedToUnlock
import repositories.LockRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal

private[services] class LockService @Inject()(
  lockRepository: LockRepository
)(implicit ec: ExecutionContext) {

  private def lockDeparture(departureId: DepartureId)(implicit ec: ExecutionContext): EitherT[Future, ErrorState, Unit] =
    EitherT(lockRepository.lock(departureId).map {
      case true  => Right(())
      case false => Left(DepartureAlreadyLocked(departureId))
    } recover {
      case NonFatal(e) => Left(new FailedToLock(departureId, e))
    })

  private def unlockDeparture(departureId: DepartureId)(implicit ec: ExecutionContext): EitherT[Future, ErrorState, Unit] =
    EitherT(lockRepository.unlock(departureId).map(Right.apply) recover {
      case NonFatal(e) => Left(new FailedToUnlock(departureId, e))
    })

  def withLock[T](departureId: DepartureId)(action: => EitherT[Future, ErrorState, T]): EitherT[Future, ErrorState, T] =
    (for {
      _       <- lockDeparture(departureId)
      perform <- action
      _       <- unlockDeparture(departureId)
    } yield perform) leftFlatMap {
      case e @ DepartureAlreadyLocked(_) => EitherT.fromEither(Left(e))
      case e                             => unlockDeparture(departureId).transform(_ => Left(e))
    }
}
