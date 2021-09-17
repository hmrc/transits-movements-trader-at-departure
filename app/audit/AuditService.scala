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

package audit

import audit.AuditType._
import cats.data.Ior
import models._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.MessageTranslation
import utils.XmlToJson

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq

class AuditService @Inject()(auditConnector: AuditConnector, messageTranslator: MessageTranslation)(implicit ec: ExecutionContext) extends XmlToJson {

  def auditEvent(auditType: AuditType, customerId: Ior[TURN, EORINumber], message: Message, channel: ChannelType)(implicit hc: HeaderCarrier): Unit = {
    val details = AuthenticatedAuditDetails(channel, customerId, messageTranslator.translate(message.messageJson))
    auditConnector.sendExplicitAudit(auditType.toString, details)
  }

  def auditNCTSMessages(channel: ChannelType, customerId: String, messageResponse: MessageResponse, xml: NodeSeq)(implicit hc: HeaderCarrier): Unit = {

    val auditType: AuditType = messageResponse match {
      case PositiveAcknowledgementResponse              => PositiveAcknowledgementReceived
      case MrnAllocatedResponse                         => MrnAllocatedReceived
      case DepartureRejectedResponse                    => DeclarationRejectedReceived
      case ControlDecisionNotificationResponse          => ControlDecisionNotificationReceived
      case NoReleaseForTransitResponse                  => NoReleaseForTransitReceived
      case ReleaseForTransitResponse                    => ReleaseForTransitReceived
      case CancellationDecisionResponse                 => CancellationDecisionReceived
      case WriteOffNotificationResponse                 => WriteOffNotificationReceived
      case GuaranteeNotValidResponse                    => GuaranteeNotValidReceived
      case XMLSubmissionNegativeAcknowledgementResponse => XMLSubmissionNegativeAcknowledgement
    }

    val details = UnauthenticatedAuditDetails(channel, customerId, toJson(xml))

    auditConnector.sendExplicitAudit(auditType.toString, details)
  }

}
