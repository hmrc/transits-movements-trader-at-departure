/*
 * Copyright 2020 HM Revenue & Customs
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

package controllers.actions

import javax.inject.Inject
import models.DepartureId
import play.api.mvc._
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.Locked
import repositories.LockRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class LockActionProvider @Inject()(
  lockRepository: LockRepository,
  ec: ExecutionContext,
  parser: BodyParsers.Default
) {

  def apply(departureId: DepartureId): ActionBuilder[Request, AnyContent] with ActionFunction[Request, Request] =
    new LockAction(departureId, ec, lockRepository, parser)
}

private[actions] class LockAction(
  departureId: DepartureId,
  implicit protected val executionContext: ExecutionContext,
  lockRepository: LockRepository,
  val parser: BodyParsers.Default
) extends ActionBuilder[Request, AnyContent]
    with ActionFunction[Request, Request] {

  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    lockRepository.lock(departureId).flatMap {
      case true => {
        block(request)
          .flatMap {
            result =>
              lockRepository.unlock(departureId).map {
                _ =>
                  result
              }
          }
          .recoverWith {
            case e: Exception =>
              lockRepository.unlock(departureId).map {
                _ =>
                  InternalServerError
              }
          }
      }
      case false =>
        Future.successful(Locked)
    }
}
