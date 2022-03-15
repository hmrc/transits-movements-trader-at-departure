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
                                   requestSize: Int,
                                   message: NodeSeq,
                                   messageTranslator: MessageTranslation) {

  def customerId: String =
    enrolmentId.fold(
      turn => turn.value,
      eoriNumber => eoriNumber.value,
      (_, eoriNumber) => eoriNumber.value
    )

  def enrolmentType: String =
    enrolmentId.fold(
      _ => Constants.LegacyEnrolmentKey,
      _ => Constants.NewEnrolmentKey,
      (_, _) => Constants.NewEnrolmentKey
    )

  def statistics: JsObject = Json.obj(
    "itemCount"   -> (message \\ "TotNumOfIteHEA305").text.toInt,
    "requestSize" -> requestSize
  )

  def declaration: JsObject =
    if (requestSize > DeclarationAuditDetails.maxContentLength) Json.obj("declaration" -> "The declaration data was too large to be included")
    else messageTranslator.translate(toJson(message))

}

object DeclarationAuditDetails {

  private val maxContentLength = 20000

  implicit val writes: OWrites[DeclarationAuditDetails] = (details: DeclarationAuditDetails) => {
    Json.obj(
      "channel" -> details.channel,
      "customerId"    -> details.customerId,
      "enrolmentType" -> details.enrolmentType,
      "message"       -> details.declaration,
      "statistics"    -> details.statistics
    )
  }
}
