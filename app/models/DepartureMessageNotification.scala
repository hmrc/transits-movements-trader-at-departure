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

import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.NodeSeqFormat._
import models.request.DepartureResponseRequest

import java.time.LocalDateTime
import scala.xml.NodeSeq

case class DepartureMessageNotification(messageUri: String,
                                        requestId: String,
                                        departureId: DepartureId,
                                        messageId: MessageId,
                                        received: LocalDateTime,
                                        messageType: MessageType)

object DepartureMessageNotification {

  private def requestId(departureId: DepartureId): String = s"/customs/transits/movements/departures/${departureId.index}"

  implicit val writesDepartureId: Writes[DepartureId] = Writes.of[String].contramap(_.index.toString)

  private val writesDepartureMessageNotification: OWrites[DepartureMessageNotification] =
    (
      (__ \ "messageUri").write[String] and
        (__ \ "requestId").write[String] and
        (__ \ "departureId").write[DepartureId] and
        (__ \ "messageId").write[MessageId] and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType]
    )(unlift(DepartureMessageNotification.unapply))

  implicit val writesDepartureMessageNotificationWithRequestId: OWrites[DepartureMessageNotification] =
    OWrites.transform(writesDepartureMessageNotification) {
      (departure, obj) =>
        obj ++ Json.obj("requestId" -> requestId(departure.departureId))
    }

  def fromRequest(request: DepartureResponseRequest[NodeSeq], timestamp: LocalDateTime): DepartureMessageNotification = {
    val messageId    = request.departure.nextMessageId
    val departureUrl = requestId(request.departure.departureId)
    DepartureMessageNotification(
      s"$departureUrl/messages/${messageId.value}",
      departureUrl,
      request.departure.departureId,
      messageId,
      timestamp,
      request.messageResponse.messageType
    )
  }
}
