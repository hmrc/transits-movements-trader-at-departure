/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.Ior
import config.Constants
import models.ChannelType
import models.EORINumber
import models.TURN
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import utils.MessageTranslation
import utils.XMLTransformer.toJson

import scala.xml.NodeSeq

case class DeclarationAuditDetails(channel: ChannelType,
                                   enrolmentId: Ior[TURN, EORINumber],
                                   message: NodeSeq,
                                   requestLength: Int,
                                   messageTranslator: MessageTranslation) {

  lazy val customerId: String =
    enrolmentId.fold(
      turn => turn.value,
      eoriNumber => eoriNumber.value,
      (_, eoriNumber) => eoriNumber.value
    )

  lazy val enrolmentType: String =
    enrolmentId.fold(
      _ => Constants.LegacyEnrolmentKey,
      _ => Constants.NewEnrolmentKey,
      (_, _) => Constants.NewEnrolmentKey
    )

  def fieldOccurrenceCount(field: String): Int = (message \\ field).length
  def fieldValue(field: String): String        = if (fieldOccurrenceCount(field) == 0) "NULL" else (message \\ field).text

  lazy val statistics: JsObject = Json.obj(
    "consignor1"                       -> fieldValue("TRACONCO1"),
    "consignor2"                       -> fieldValue("TRACONCO2"),
    "consignee1"                       -> fieldValue("TRACONCE1"),
    "consignee2"                       -> fieldValue("TRACONCE2"),
    "principalTrader"                  -> fieldValue("TRAPRIPC1"),
    "guaranteeReferenceNumber"         -> fieldValue("GuaRefNumGRNREF1"),
    "guaranteeAccessCode"              -> fieldValue("AccCodREF6"),
    "packageCount"                     -> fieldValue("TotNumOfPacHEA306"),
    "itemCount"                        -> fieldValue("TotNumOfIteHEA305"),
    "totalNoOfTransitOffices"          -> fieldOccurrenceCount("CUSOFFTRARNS"),
    "totalNoOfSeals"                   -> fieldOccurrenceCount("SEAIDSID"),
    "totalNoOfGuarantees"              -> fieldOccurrenceCount("GUAREFREF"),
    "totalNoOfPreviousAdminReferences" -> fieldOccurrenceCount("PREADMREFAR2"),
    "totalNoOfProducedDocs"            -> fieldOccurrenceCount("PRODOCDC2"),
    "totalNoOfSpecialMentions"         -> fieldOccurrenceCount("SPEMENMT2"),
    "totalNoOfContainers"              -> fieldOccurrenceCount("CONNR2"),
    "totalNoOfCountriesOfRouting"      -> fieldOccurrenceCount("CouOfRouCodITI1"),
    "requestLength"                      -> requestLength
  )

  lazy val declaration: JsObject =
    if (requestLength > DeclarationAuditDetails.maxRequestLength) Json.obj("declaration" -> "Departure declaration too large to be included")
    else messageTranslator.translate(toJson(message))

}

object DeclarationAuditDetails {

  val maxRequestLength = 20000

  implicit val writes: OWrites[DeclarationAuditDetails] = (details: DeclarationAuditDetails) => {
    Json.obj(
      "channel"       -> details.channel,
      "customerId"    -> details.customerId,
      "enrolmentType" -> details.enrolmentType,
      "message"       -> details.declaration,
      "statistics"    -> details.statistics
    )
  }
}
