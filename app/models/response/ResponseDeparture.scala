/*
 * Copyright 2020 HM Revenue & Customs
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

import java.time.LocalDateTime

import controllers.routes
import models.{Departure, DepartureId, DepartureStatus, MovementReferenceNumber}
import play.api.libs.json.{Json, OWrites}

case class ResponseDeparture(departureId: DepartureId,
                             location: String,
                             messagesLocation: String,
                             movementReferenceNumber: Option[MovementReferenceNumber],
                             referenceNumber: String,
                             status: DepartureStatus,
                             created: LocalDateTime,
                             updated: LocalDateTime)

object ResponseDeparture {

  def build(departure: Departure): ResponseDeparture =
    ResponseDeparture(
      departure.departureId,
      routes.DeparturesController.get(departure.departureId).url,
      routes.MessagesController.getMessages(departure.departureId).url,
      departure.movementReferenceNumber,
      departure.referenceNumber,
      departure.status,
      departure.created,
      departure.updated
    )

  implicit val writes: OWrites[ResponseDeparture] = Json.writes[ResponseDeparture]

}
