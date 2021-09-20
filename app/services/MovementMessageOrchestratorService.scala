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

import audit.AuditService
import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture
import com.kenshoo.play.metrics.Metrics
import metrics.HasMetrics
import models.Departure
import models.DepartureStatus
import models.MessageResponse
import models.MessageSender
import models.MrnAllocatedResponse
import models.StatusTransition
import models.ErrorState
import models.SubmissionSuccess
import models.XMLMRNError
import play.api.Logging
import play.api.mvc.Request
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementMessageOrchestratorService @Inject()(
  lockService: LockService,
  getDepartureService: DepartureRetrievalService,
  saveMessageService: SaveMessageService,
  auditService: AuditService,
  pushPullNotificationService: PushPullNotificationService,
  val metrics: Metrics,
)(implicit ec: ExecutionContext)
    extends HasMetrics
    with Logging {

  private def validateAndSaveMessage(messageResponse: MessageResponse,
                                     xml: NodeSeq,
                                     departure: Departure,
                                     messageSender: MessageSender,
                                     newState: DepartureStatus): EitherT[Future, ErrorState, SubmissionSuccess] =
    messageResponse match {
      case MrnAllocatedResponse =>
        for {
          mrn <- EitherT.fromEither(XmlMessageParser.mrnR(xml).left.map[ErrorState](error => XMLMRNError(error.message)))
          savedMessage <- EitherT(
            saveMessageService
              .validateXmlSaveMessageUpdateMrn(departure.nextMessageId, xml, messageSender, messageResponse, newState, mrn)
              .map(_.toEither(departure)))
        } yield savedMessage
      case _ =>
        EitherT(saveMessageService.validateXmlAndSaveMessage(departure.nextMessageId, xml, messageSender, messageResponse, newState).map(_.toEither(departure)))
    }

  def saveNCTSMessage(messageSender: MessageSender)(
    implicit hc: HeaderCarrier,
    request: Request[NodeSeq]
  ): Future[Either[ErrorState, SubmissionSuccess]] =
    lockService
      .withLock(messageSender.departureId)(for {
        inboundMessageResponse <- EitherT.fromEither(MessageResponse.fromRequest(request))
        departure              <- getDepartureService.getDepartureAndAuditDeletedDepartures(messageSender.departureId, inboundMessageResponse, request.body)
        nextStatus             <- EitherT.fromEither(StatusTransition.targetStatus(departure.status, inboundMessageResponse.messageReceived))
        validatedMessage       <- validateAndSaveMessage(inboundMessageResponse, request.body, departure, messageSender, nextStatus)
        _ = auditService.auditNCTSMessages(departure.channel, departure.eoriNumber, inboundMessageResponse, request.body)
        _ = pushPullNotificationService.sendPushNotificationIfBoxExists(departure, inboundMessageResponse, request)
      } yield validatedMessage)
      .value
}
