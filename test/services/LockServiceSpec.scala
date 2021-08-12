/*
 * Copyright 2021 HM Revenue & Customs
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

import base.SpecBase
import cats.data.EitherT
import models.DepartureAlreadyLocked
import models.DepartureId
import models.FailedToLock
import models.FailedToUnlock
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.verifyNoInteractions
import org.mockito.Mockito.when
import repositories.LockRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class LockServiceSpec extends SpecBase {

  class Setup {
    val mockRepo: LockRepository = mock[LockRepository]

    val departureId: DepartureId = DepartureId(1)

    lazy val service = new LockService(mockRepo)
  }

  "withLock" - {
    "must perform the action if lock is acquired" in new Setup {
      when(mockRepo.lock(eqTo(departureId))).thenReturn(Future.successful(true))
      when(mockRepo.unlock(eqTo(departureId))).thenReturn(Future.successful(()))

      service.withLock(departureId)(EitherT.fromEither(Right("Hello"))).value.futureValue mustBe Right("Hello")

      verify(mockRepo).lock(eqTo(departureId))
      verify(mockRepo).unlock(eqTo(departureId))
    }

    "return a DepartureAlreadyLocked if the departure is already locked" in new Setup {
      when(mockRepo.lock(eqTo(departureId))).thenReturn(Future.successful(false))
      when(mockRepo.unlock(eqTo(departureId))).thenReturn(Future.successful(()))

      service.withLock(departureId)(EitherT.fromEither(Right("Hello"))).value.futureValue mustBe Left(DepartureAlreadyLocked(departureId))

      verify(mockRepo).lock(eqTo(departureId))
      verify(mockRepo, never()).unlock(eqTo(departureId))
    }

    "return a FailedToLock an exception happens when trying to acquire a lock" in new Setup {
      val exception: Exception = new Exception("hello")

      when(mockRepo.lock(eqTo(departureId))).thenReturn(Future.failed(exception))
      when(mockRepo.unlock(eqTo(departureId))).thenReturn(Future.successful(()))

      service.withLock(departureId)(EitherT.fromEither(Right("Hello"))).value.futureValue mustBe Left(FailedToLock(departureId, exception))

      verify(mockRepo).lock(eqTo(departureId))
      verify(mockRepo).unlock(eqTo(departureId))
    }

    "return a FailedToUnlock if an exception happens when trying to unlock the record" in new Setup {
      val exception: Exception = new Exception("hello")

      when(mockRepo.lock(eqTo(departureId))).thenReturn(Future.successful(true))
      when(mockRepo.unlock(eqTo(departureId))).thenReturn(Future.failed(exception), Future.successful(()))

      service.withLock(departureId)(EitherT.fromEither(Right("Hello"))).value.futureValue mustBe Left(FailedToUnlock(departureId, exception))

      verify(mockRepo).lock(eqTo(departureId))
      verify(mockRepo, times(2))
        .unlock(eqTo(departureId))
    }
  }
}
