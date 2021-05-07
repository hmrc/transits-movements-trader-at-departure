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

package controllers

import javax.inject.Inject
import audit.AuditService
import audit.AuditType._
import com.kenshoo.play.metrics.Metrics
import controllers.actions._
import models._
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import metrics.HasActionMetrics
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.DepartureRepository
import services._
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class DeparturesController @Inject() (
  cc: ControllerComponents,
  departureRepository: DepartureRepository,
  authenticate: AuthenticateActionProvider,
  authenticateClientId: AuthenticatedClientIdActionProvider,
  authenticatedDepartureForRead: AuthenticatedGetDepartureForReadActionProvider,
  departureService: DepartureService,
  auditService: AuditService,
  submitMessageService: SubmitMessageService,
  pushPullNotificationService: PushPullNotificationService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  lazy val departuresCount = histo("get-all-departures-count")
  lazy val messagesCount   = histo("get-departure-by-id-messages-count")

  def post: Action[NodeSeq] =
    withMetricsTimerAction("post-create-departure") {
      authenticateClientId().async(parse.xml) {
        implicit request =>
          pushPullNotificationService.getBox(request.clientId).flatMap {
            boxOpt =>
              departureService
                .createDeparture(request.eoriNumber, request.body, request.channel, boxOpt)
                .flatMap {
                  case Left(error) =>
                    logger.error(error.message)
                    Future.successful(BadRequest(error.message))
                  case Right(departure) =>
                    submitMessageService
                      .submitDeparture(departure)
                      .map {
                        case SubmissionProcessingResult.SubmissionFailureInternal =>
                          InternalServerError
                        case SubmissionProcessingResult.SubmissionFailureExternal =>
                          BadGateway
                        case submissionFailureRejected: SubmissionProcessingResult.SubmissionFailureRejected =>
                          BadRequest(submissionFailureRejected.responseBody)
                        case SubmissionProcessingResult.SubmissionSuccess =>
                          auditService.auditEvent(DepartureDeclarationSubmitted, departure.messages.head, request.channel)
                          auditService.auditEvent(MesSenMES3Added, departure.messages.head, request.channel)
                          Accepted(Json.toJson(boxOpt))
                            .withHeaders(
                              "Location" -> routes.DeparturesController.get(departure.departureId).url
                            )
                      }
                      .recover {
                        case _ =>
                          InternalServerError
                      }
                }
                .recover {
                  case _ =>
                    InternalServerError
                }
          }
      }
    }

  def get(departureId: DepartureId): Action[AnyContent] =
    withMetricsTimerAction("get-departure-by-id") {
      authenticatedDepartureForRead(departureId) {
        implicit request =>
          messagesCount.update(request.departure.messages.length)
          Ok(Json.toJsObject(ResponseDeparture.build(request.departure)))
      }
    }

  def getDepartures(): Action[AnyContent] =
    withMetricsTimerAction("get-all-departures") {
      authenticate().async {
        implicit request =>
          departureRepository
            .fetchAllDepartures(request.eoriNumber, request.channel)
            .map {
              allDepartures =>
                departuresCount.update(allDepartures.length)
                Ok(Json.toJsObject(ResponseDepartures(allDepartures.map {
                  departure =>
                    ResponseDeparture.build(departure)
                })))
            }
            .recover {
              case e =>
                logger.error(s"Failed to create departure", e)
                InternalServerError
            }
      }
    }
}
