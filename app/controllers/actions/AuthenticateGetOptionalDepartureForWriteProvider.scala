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

import com.google.inject.ImplementedBy
import javax.inject.Inject
import play.api.mvc.{ActionBuilder, ActionFunction, AnyContent, Result}
import models.request.{AuthenticatedOptionalDepartureRequest, AuthenticatedRequest}
import play.api.Logger
import play.api.mvc.Results.{BadRequest, InternalServerError, Locked}
import repositories.{DepartureRepository, LockRepository}
import services.XmlMessageParser

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

trait AuthenticateGetOptionalDepartureForWriteActionProvider {
  def apply(): ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent]
}

class AuthenticateGetOptionalDepartureForWriteActionProviderImpl @Inject()(
       authenticate: AuthenticateActionProvider,
       departureRepository: DepartureRepository,
       lockRepository: LockRepository,
       ec: ExecutionContext
   ) extends AuthenticateGetOptionalDepartureForWriteActionProvider {

  def apply(): ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent] =
    authenticate() andThen new AuthenticateGetOptionalDepartureForWriteAction(departureRepository, lockRepository, ec)
}

class AuthenticateGetOptionalDepartureForWriteAction(departureRepository: DepartureRepository,
                                                     lockRepository: LockRepository,
                                                     implicit protected val executionContext: ExecutionContext) extends ActionFunction[AuthenticatedRequest, AuthenticatedOptionalDepartureRequest] {
  override def invokeBlock[A](request: AuthenticatedRequest[A], block: AuthenticatedOptionalDepartureRequest[A] => Future[Result]): Future[Result] =
    request.body match {
      case body: NodeSeq =>
        XmlMessageParser.referenceR(body) match {
          case None =>
            Logger.warn("Invalid mrn specified in request")
            Future.successful(BadRequest("Invalid mrn specified in request"))

          case Some(reference) => {
            departureRepository.get(request.eoriNumber, reference).flatMap {
              case None => block(AuthenticatedOptionalDepartureRequest(request, None, request.eoriNumber))
              case Some(departure) =>
                lockRepository.lock(departure.departureId).flatMap {
                  case false => Future.successful(Locked)
                  case true =>
                    block(AuthenticatedOptionalDepartureRequest(request, Some(departure), request.eoriNumber))
                      .flatMap {
                        result =>
                          lockRepository.unlock(departure.departureId).map {
                            _ =>
                              result
                          }
                      }
                      .recoverWith {
                        case e: Exception =>
                          lockRepository.unlock(departure.departureId).map {
                            _ =>
                              InternalServerError
                          }
                      }
                }
            }
          }
        }
      case invalidBody =>
        Logger.warn(s"Invalid request body: ${invalidBody.getClass}")
        Future.successful(BadRequest(s"Invalid request body: ${invalidBody.getClass}"))
    }
}