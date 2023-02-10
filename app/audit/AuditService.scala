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

package audit

import audit.AuditType._
import models._
import models.request.AuthenticatedRequest
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import utils.MessageTranslation
import utils.XMLTransformer.toJson

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.xml.NodeSeq

object AuditService {

  val maxRequestLength: Int = 20000

}

class AuditService @Inject()(auditConnector: AuditConnector, messageTranslator: MessageTranslation)(implicit ec: ExecutionContext) {

  def authAudit(auditType: AuditType, details: AuthenticationDetails)(implicit hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType.toString, details)

  def auditEvent(auditType: AuditType, enrolmentId: EnrolmentId, message: Message, channel: ChannelType)(implicit hc: HeaderCarrier): Unit = {
    val details = AuthenticatedAuditDetails(channel, enrolmentId.customerId, enrolmentId.enrolmentType, messageTranslator.translate(toJson(message.message)))
    auditConnector.sendExplicitAudit(auditType.toString, details)
  }

  def auditCustomerRequestedMissingMovementEvent(request: AuthenticatedRequest[_], departureId: DepartureId): Unit = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
    val details =
      AuthenticatedAuditDetails(request.channel, request.enrolmentId.customerId, request.enrolmentId.enrolmentType, Json.obj("departureId" -> departureId))

    auditConnector.sendExplicitAudit(CustomerRequestedMissingMovement.toString, details)
  }

  def auditNCTSRequestedMissingMovementEvent(departureId: DepartureId, messageResponse: MessageResponse, xml: NodeSeq)(implicit hc: HeaderCarrier): Unit = {
    val details = Json.obj(
      "departureId"         -> departureId,
      "messageResponseType" -> getAuditType(messageResponse).toString,
      "message"             -> toJson(xml)
    )
    auditConnector.sendExplicitAudit(NCTSRequestedMissingMovement.toString, details)
  }

  def auditNCTSMessages(channel: ChannelType, customerId: String, messageResponse: MessageResponse, xml: NodeSeq)(implicit hc: HeaderCarrier): Unit = {

    val auditType = getAuditType(messageResponse)

    val details = UnauthenticatedAuditDetails(channel, customerId, toJson(xml))

    auditConnector.sendExplicitAudit(auditType.toString, details)
  }

  private def getAuditType(messageResponse: MessageResponse): AuditType = messageResponse match {
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

  def auditDeclarationWithStatistics(
    auditType: AuditType,
    customerId: String,
    enrolmentType: String,
    message: Message,
    channel: ChannelType,
    requestLength: Int,
    boxOpt: Option[BoxId]
  )(implicit hc: HeaderCarrier): Unit = {

    val messageNodes: NodeSeq = message.message
    val statistics: JsObject = Json.obj(
      "consignor1"                       -> fieldValue(messageNodes, "TRACONCO1"),
      "consignor2"                       -> fieldValue(messageNodes, "TRACONCO2"),
      "consignee1"                       -> fieldValue(messageNodes, "TRACONCE1"),
      "consignee2"                       -> fieldValue(messageNodes, "TRACONCE2"),
      "principalTrader"                  -> fieldValue(messageNodes, "TRAPRIPC1"),
      "guaranteeReferenceNumber"         -> fieldValue(messageNodes, "GuaRefNumGRNREF1"),
      "guaranteeAccessCode"              -> fieldValue(messageNodes, "AccCodREF6"),
      "packageCount"                     -> fieldValue(messageNodes, "TotNumOfPacHEA306"),
      "itemCount"                        -> fieldValue(messageNodes, "TotNumOfIteHEA305"),
      "totalNoOfTransitOffices"          -> fieldOccurrenceCount(messageNodes, "CUSOFFTRARNS"),
      "totalNoOfSeals"                   -> fieldOccurrenceCount(messageNodes, "SEAIDSID"),
      "totalNoOfGuarantees"              -> fieldOccurrenceCount(messageNodes, "GUAREFREF"),
      "totalNoOfPreviousAdminReferences" -> fieldOccurrenceCount(messageNodes, "PREADMREFAR2"),
      "totalNoOfProducedDocs"            -> fieldOccurrenceCount(messageNodes, "PRODOCDC2"),
      "totalNoOfSpecialMentions"         -> fieldOccurrenceCount(messageNodes, "SPEMENMT2"),
      "totalNoOfContainers"              -> fieldOccurrenceCount(messageNodes, "CONNR2"),
      "totalNoOfCountriesOfRouting"      -> fieldOccurrenceCount(messageNodes, "CouOfRouCodITI1"),
      "requestLength"                    -> requestLength
    )

    val json: JsObject =
      if (requestLength > AuditService.maxRequestLength) Json.obj("declaration" -> "Departure declaration too large to be included")
      else messageTranslator.translate(toJson(message.message))

    val details = DeclarationAuditDetails(channel, customerId, enrolmentType, json, statistics, boxOpt)

    auditConnector.sendExplicitAudit(auditType.toString, details)
  }

  private def fieldValue(message: NodeSeq, field: String): String =
    if (fieldOccurrenceCount(message, field) == 0) "NULL" else (message \\ field).text

  private def fieldOccurrenceCount(message: NodeSeq, field: String): Int = (message \\ field).length
}
