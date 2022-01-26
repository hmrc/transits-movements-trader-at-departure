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

package models.response

import controllers.routes
import models.DepartureId
import models.DepartureStatus
import models.DepartureWithoutMessages
import models.MovementReferenceNumber
import play.api.libs.json.Json
import play.api.libs.json.OWrites

import java.time.LocalDateTime

case class ResponseDeparture(departureId: DepartureId,
                             location: String,
                             messagesLocation: String,
                             movementReferenceNumber: Option[MovementReferenceNumber],
                             referenceNumber: String,
                             status: DepartureStatus,
                             previousStatus: DepartureStatus,
                             created: LocalDateTime,
                             updated: LocalDateTime)

object ResponseDeparture {

  def fromDepartureWithoutMessage(departureWithoutMessages: DepartureWithoutMessages): ResponseDeparture =
    ResponseDeparture(
      departureWithoutMessages.departureId,
      routes.DeparturesController.get(departureWithoutMessages.departureId).url,
      routes.MessagesController.getMessages(departureWithoutMessages.departureId).url,
      departureWithoutMessages.movementReferenceNumber,
      departureWithoutMessages.referenceNumber,
      departureWithoutMessages.status,
      departureWithoutMessages.previousStatus,
      departureWithoutMessages.created,
      departureWithoutMessages.lastUpdated
    )

  implicit val writes: OWrites[ResponseDeparture] = Json.writes[ResponseDeparture]

}
