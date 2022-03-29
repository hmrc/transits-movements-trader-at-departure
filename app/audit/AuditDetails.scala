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

import models.BoxId
import models.ChannelType
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.functional.syntax.unlift
import play.api.libs.json.JsObject
import play.api.libs.json.OWrites
import play.api.libs.json.__

sealed abstract class AuditDetails {
  def channel: ChannelType
  def customerId: String
  def json: JsObject
}

case class AuthenticatedAuditDetails(channel: ChannelType, customerId: String, enrolmentType: String, json: JsObject) extends AuditDetails

object AuthenticatedAuditDetails {

  implicit val writes: OWrites[AuthenticatedAuditDetails] = (
    (__ \ "channel").write[ChannelType] and
      (__ \ "customerId").write[String] and
      (__ \ "enrolmentType").write[String] and
      (__ \ "json").write[JsObject]
  )(unlift(AuthenticatedAuditDetails.unapply))

}

case class UnauthenticatedAuditDetails(channel: ChannelType, customerId: String, json: JsObject) extends AuditDetails

object UnauthenticatedAuditDetails {

  implicit val writes: OWrites[UnauthenticatedAuditDetails] = (
    (__ \ "channel").write[ChannelType] and
      (__ \ "customerId").write[String] and
      (__ \ "json").write[JsObject]
  )(unlift(UnauthenticatedAuditDetails.unapply))
}

case class DeclarationAuditDetails(channel: ChannelType,
                                   customerId: String,
                                   enrolmentType: String,
                                   message: JsObject,
                                   statistics: JsObject,
                                   boxId: Option[BoxId])

object DeclarationAuditDetails {

  implicit val writes: OWrites[DeclarationAuditDetails] = (
    (__ \ "channel").write[ChannelType] and
      (__ \ "customerId").write[String] and
      (__ \ "enrolmentType").write[String] and
      (__ \ "message").write[JsObject] and
      (__ \ "statistics").write[JsObject] and
      (__ \ "boxId").writeNullable[BoxId]
  )(unlift(DeclarationAuditDetails.unapply))
}
