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
import models._
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.MessageTranslation
import utils.XmlToJson

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq

class AuditService @Inject()(auditConnector: AuditConnector, messageTranslator: MessageTranslation)(implicit ec: ExecutionContext) extends XmlToJson {

  def auditEvent(auditType: AuditType, message: Message, channel: ChannelType)(implicit hc: HeaderCarrier): Unit =
    internal_auditEvent(auditType, message.message, message.messageJson, channel)

  private def internal_auditEvent(auditType: AuditType, xml: NodeSeq, json: JsObject, channel: ChannelType)(implicit hc: HeaderCarrier) = {
    val details = AuditDetails(channel, messageTranslator.translate(json), xml.toString())
    auditConnector.sendExplicitAudit(auditType.toString(), Json.toJson(details))
  }

  def auditNCTSMessages(channel: ChannelType, messageResponse: MessageResponse, xml: NodeSeq)(implicit hc: HeaderCarrier): Unit = {
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
    internal_auditEvent(auditType, xml, toJson(xml), channel)
  }

}
