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

import cats.data.Ior
import config.Constants
import models.ChannelType
import models.EORINumber
import models.TURN
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.OWrites

sealed abstract class AuditDetails {
  def channel: ChannelType
  def customerId: String
  def json: JsObject
}

case class AuthenticatedAuditDetails(channel: ChannelType, enrolmentId: Ior[TURN, EORINumber], json: JsObject) extends AuditDetails {

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
}

object AuthenticatedAuditDetails {

  implicit val writes: OWrites[AuthenticatedAuditDetails] = (details: AuthenticatedAuditDetails) => {
    Json.obj(
      "channel"       -> details.channel,
      "customerId"    -> details.customerId,
      "enrolmentType" -> details.enrolmentType
    ) ++ details.json
  }
}

case class UnauthenticatedAuditDetails(channel: ChannelType, customerId: String, json: JsObject) extends AuditDetails

object UnauthenticatedAuditDetails {

  implicit val writes: OWrites[UnauthenticatedAuditDetails] = (details: UnauthenticatedAuditDetails) => {
    Json.obj(
      "channel"    -> details.channel,
      "customerId" -> details.customerId
    ) ++ details.json
  }
}
