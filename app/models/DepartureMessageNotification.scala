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

package models

import models.request.DepartureResponseRequest
import play.api.http.HeaderNames
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.NodeSeqFormat._

import java.time.LocalDateTime
import scala.xml.NodeSeq

case class DepartureMessageNotification(
  messageUri: String,
  requestId: String,
  departureId: DepartureId,
  messageId: MessageId,
  received: LocalDateTime,
  messageType: MessageType,
  messageBody: Option[NodeSeq]
)

object DepartureMessageNotification {

  private def requestId(departureId: DepartureId): String = s"/customs/transits/movements/departures/${departureId.index}"

  implicit val writesDepartureId: Writes[DepartureId] = Writes.of[String].contramap(_.index.toString)

  private val writesDepartureMessageNotification: OWrites[DepartureMessageNotification] =
    (
      (__ \ "messageUri").write[String] and
        (__ \ "requestId").write[String] and
        (__ \ "departureId").write[DepartureId] and
        (__ \ "messageId").write[String].contramap[MessageId](_.publicValue.toString) and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType] and
        (__ \ "messageBody").writeNullable[NodeSeq]
    )(unlift(DepartureMessageNotification.unapply))

  implicit val writesDepartureMessageNotificationWithRequestId: OWrites[DepartureMessageNotification] =
    OWrites.transform(writesDepartureMessageNotification) {
      (departure, obj) =>
        obj ++ Json.obj("requestId" -> requestId(departure.departureId))
    }

  def fromRequest(request: DepartureResponseRequest[NodeSeq], timestamp: LocalDateTime): DepartureMessageNotification = {
    val oneHundredKilobytes = 100000
    val messageId           = MessageId.fromIndex(request.departure.messages.length)
    val departureUrl        = requestId(request.departure.departureId)
    val bodySize            = request.headers.get(HeaderNames.CONTENT_LENGTH).map(_.toInt)
    DepartureMessageNotification(
      s"$departureUrl/messages/${messageId.publicValue}",
      departureUrl,
      request.departure.departureId,
      messageId,
      timestamp,
      request.messageResponse.messageType,
      if (bodySize.exists(_ <= oneHundredKilobytes)) Some(request.body) else None
    )
  }
}
