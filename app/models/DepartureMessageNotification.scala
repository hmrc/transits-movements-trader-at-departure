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

package models

import models.request.DepartureResponseRequest
import play.api.http.HeaderNames
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.Request
import utils.NodeSeqFormat._

import java.time.LocalDateTime
import scala.xml.NodeSeq

case class DepartureMessageNotification(
  messageUri: String,
  requestId: String,
  customerId: String,
  departureId: DepartureId,
  messageId: MessageId,
  received: LocalDateTime,
  messageType: MessageType,
  messageBody: Option[NodeSeq]
)

object DepartureMessageNotification {

  private def requestId(departureId: DepartureId): String = s"/customs/transits/movements/departures/${departureId.index}"

  implicit val writesDepartureId: Writes[DepartureId] = Writes.of[String].contramap(_.index.toString)

  implicit val writesDepartureMessageNotification: OWrites[DepartureMessageNotification] =
    (
      (__ \ "messageUri").write[String] and
        (__ \ "requestId").write[String] and
        (__ \ "customerId").write[String] and
        (__ \ "departureId").write[DepartureId] and
        (__ \ "messageId").write[MessageId] and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType] and
        (__ \ "messageBody").writeNullable[NodeSeq]
    )(unlift(DepartureMessageNotification.unapply))

  def fromRequest(request: DepartureResponseRequest[NodeSeq], timestamp: LocalDateTime): DepartureMessageNotification = {
    val eightyKilobytes = 80000
    val eoriNumber      = request.departure.eoriNumber
    val messageId       = request.departure.nextMessageId
    val departureUrl    = requestId(request.departure.departureId)
    val bodySize        = request.headers.get(HeaderNames.CONTENT_LENGTH).map(_.toInt)
    DepartureMessageNotification(
      s"$departureUrl/messages/${messageId.value}",
      departureUrl,
      eoriNumber,
      request.departure.departureId,
      messageId,
      timestamp,
      request.messageResponse.messageType,
      if (bodySize.exists(_ < eightyKilobytes)) Some(request.body) else None
    )
  }

  def fromDepartureAndResponse(
    departure: Departure,
    messageResponse: MessageResponse,
    timestamp: LocalDateTime,
    request: Request[NodeSeq]
  ): DepartureMessageNotification = {
    val eightyKilobytes = 80000
    val messageId       = departure.nextMessageId
    val departureUrl    = requestId(departure.departureId)
    val bodySize        = request.headers.get(HeaderNames.CONTENT_LENGTH).map(_.toInt)

    DepartureMessageNotification(
      s"$departureUrl/messages/${messageId.value}",
      departureUrl,
      departure.eoriNumber,
      departure.departureId,
      messageId,
      timestamp,
      messageResponse.messageType,
      if (bodySize.exists(_ < eightyKilobytes)) Some(request.body) else None
    )
  }
}
