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

package models.response

import java.time.LocalDateTime

import controllers.routes
import models.MessageStatus.SubmissionFailed
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MovementReferenceNumber
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import java.time.OffsetDateTime

case class ResponseDepartureWithMessages(departureId: DepartureId,
                                         location: String,
                                         messagesLocation: String,
                                         movementReferenceNumber: Option[MovementReferenceNumber],
                                         referenceNumber: String,
                                         status: DepartureStatus,
                                         created: LocalDateTime,
                                         updated: LocalDateTime,
                                         messages: Seq[ResponseMessage])

object ResponseDepartureWithMessages {

  def build(departure: Departure, receivedSince: Option[OffsetDateTime]): ResponseDepartureWithMessages =
    ResponseDepartureWithMessages(
      departure.departureId,
      routes.DeparturesController.get(departure.departureId).url,
      routes.MessagesController.getMessages(departure.departureId).url,
      departure.movementReferenceNumber,
      departure.referenceNumber,
      departure.status,
      departure.created,
      departure.lastUpdated,
      departure.messagesWithId
        .filterNot {
          case (message, _) =>
            val failedMessage            = message.optStatus == Some(SubmissionFailed)
            lazy val beforeRequestedDate = receivedSince.fold(false)(message.receivedBefore)
            failedMessage || beforeRequestedDate
        }
        .map {
          case (message, messageId) => ResponseMessage.build(departure.departureId, messageId, message)
        }
    )

  implicit val writes: OWrites[ResponseDepartureWithMessages] = Json.writes[ResponseDepartureWithMessages]

}
