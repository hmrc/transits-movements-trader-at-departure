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

package models

import models.Message.writes
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

case class DepartureMessages(departureId: DepartureId, eoriNumber: EORINumber, messages: List[Message])

object DepartureMessages {

  implicit val read: Reads[DepartureMessages] =
    (
      (__ \ "_id").read[DepartureId] and
        (__ \ "eoriNumber").read[EORINumber] and
        (__ \ "messages").read[List[Message]]
    )(DepartureMessages.apply _)

  implicit val write: OWrites[DepartureMessages] =
    (
      (__ \ "_id").write[DepartureId] and
        (__ \ "eoriNumber").write[EORINumber] and
        (__ \ "messages").write[List[Message]]
    )(unlift(DepartureMessages.unapply))

  implicit val formatsDeparture: Format[DepartureMessages] =
    Format(read, write)

  val projection: JsObject = Json.obj(
    "_id"        -> 1,
    "eoriNumber" -> 1,
    "messages"   -> 1
  )
}
