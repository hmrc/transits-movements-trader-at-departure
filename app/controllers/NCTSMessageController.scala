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

import com.kenshoo.play.metrics.Metrics
import controllers.actions.CheckMessageTypeActionProvider
import controllers.actions.GetDepartureWithoutMessagesForWriteActionProvider
import metrics.HasActionMetrics
import models.DepartureAlreadyLocked
import models.DepartureNotFound
import models.ExternalError
import models.MessageSender
import models.SubmissionSuccess
import models.InternalException
import models.InternalError
import play.api.Logging
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.MovementMessageOrchestratorService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import javax.inject.Inject

import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq
import audit.AuditService
import services.SaveMessageService
import services.PushPullNotificationService

class NCTSMessageController @Inject()(
  cc: ControllerComponents,
  getDepartureWithoutMessages: GetDepartureWithoutMessagesForWriteActionProvider,
  checkMessageType: CheckMessageTypeActionProvider,
  auditService: AuditService,
  saveMessageService: SaveMessageService,
  pushPullNotificationService: PushPullNotificationService,
  orchestratorService: MovementMessageOrchestratorService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  def post(messageSender: MessageSender): Action[NodeSeq] =
    withMetricsTimerAction("post-receive-ncts-message") {
      Action(parse.xml).async {
        implicit request =>
          orchestratorService.saveNCTSMessage(messageSender).map {
            case Left(_: DepartureAlreadyLocked) => Locked
            case Left(DepartureNotFound(_))      => NotFound
            case Left(ExternalError(reason)) =>
              logger.warn(s"External Submission Failure $reason")
              BadRequest(reason)
            case Left(InternalException(reason, exception)) =>
              logger.warn(reason, exception)
              InternalServerError(reason)
            case Left(InternalError(reason)) =>
              logger.warn(reason)
              InternalServerError(reason)
            case Right(SubmissionSuccess(departure)) =>
              Ok.withHeaders(
                LOCATION -> routes.MessagesController.getMessage(departure.departureId, departure.nextMessageId).url
              )
          }
      }
    }
}
