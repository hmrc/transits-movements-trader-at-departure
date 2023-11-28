/*
 * Copyright 2023 HM Revenue & Customs
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

import audit.AuditService
import audit.AuditType.DepartureDeclarationSubmitted
import audit.AuditType.MesSenMES3Added
import com.kenshoo.play.metrics.Metrics
import config.Constants
import controllers.actions._
import metrics.HasActionMetrics
import metrics.WeekLongCounter
import models._
import models.response.ResponseDeparture
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.DepartureRepository
import services._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import java.time.Clock
import java.time.OffsetDateTime
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class DeparturesController @Inject() (
  cc: ControllerComponents,
  departureRepository: DepartureRepository,
  authenticate: AuthenticateActionProvider,
  authenticatedDepartureWithoutMessagesForRead: AuthenticatedGetDepartureWithoutMessagesForReadActionProvider,
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

  lazy val acceptedDeparturesWithPPNSBox = new WeekLongCounter(Clock.systemDefaultZone(), counter("accepted-departures-with-ppns-box"))
  lazy val acceptedDepartures            = new WeekLongCounter(Clock.systemDefaultZone(), counter("accepted-departures"))

  private def getBox(clientIdOpt: Option[String])(implicit hc: HeaderCarrier): Future[Option[Box]] =
    clientIdOpt
      .map {
        clientId =>
          pushPullNotificationService.getBox(clientId)
      }
      .getOrElse(Future.successful(None))

  def post: Action[NodeSeq] =
    withMetricsTimerAction("post-create-departure") {
      authenticate().async(parse.xml) {
        implicit request =>
          getBox(request.headers.get(Constants.XClientIdHeader)).flatMap {
            boxOpt =>
              departureService
                .createDeparture(request.enrolmentId, request.body, request.channel, boxOpt)
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
                          auditService.auditDeclarationWithStatistics(
                            DepartureDeclarationSubmitted,
                            request.enrolmentId.customerId,
                            request.enrolmentId.enrolmentType,
                            departure.messages.head,
                            request.channel,
                            request.length,
                            boxOpt.map(_.boxId)
                          )
                          auditService.auditDeclarationWithStatistics(
                            MesSenMES3Added,
                            request.enrolmentId.customerId,
                            request.enrolmentId.enrolmentType,
                            departure.messages.head,
                            request.channel,
                            request.length,
                            None
                          )
                          if (boxOpt.isDefined) acceptedDeparturesWithPPNSBox.inc()
                          acceptedDepartures.inc()
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
      authenticatedDepartureWithoutMessagesForRead(departureId) {
        implicit request =>
          Ok(Json.toJsObject(ResponseDeparture.fromDepartureWithoutMessage(request.departure)))
      }
    }

  def getDepartures(updatedSince: Option[OffsetDateTime], lrn: Option[String], pageSize: Option[Int] = None, page: Option[Int] = None): Action[AnyContent] =
    withMetricsTimerAction("get-all-departures") {
      authenticate().async {
        implicit request =>
          departureRepository
            .fetchAllDepartures(request.enrolmentId.value, request.channel, updatedSince, lrn, pageSize, page)
            .map {
              responseDepartures =>
                departuresCount.update(responseDepartures.retrievedDepartures)
                Ok(Json.toJsObject(responseDepartures))
            }
            .recover {
              case e =>
                logger.error(s"Failed to create departure", e)
                InternalServerError
            }
      }
    }
}
