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
import config.Constants

case class DepartureMessageNotification(departureId: DepartureId, messageId: Int, received: LocalDateTime, messageType: MessageType, body: NodeSeq)

object DepartureMessageNotification {

  implicit val writesDepartureId: Writes[DepartureId] = Writes.of[String].contramap(_.index.toString)

  private val writesDepartureMessageNotification: OWrites[DepartureMessageNotification] =
    (
      (__ \ "departureId").write[DepartureId] and
        (__ \ "messageId").write[String].contramap[Int](_.toString) and
        (__ \ "received").write[LocalDateTime] and
        (__ \ "messageType").write[MessageType] and
        (__ \ "body").write[NodeSeq]
    )(unlift(DepartureMessageNotification.unapply))

  implicit val writesDepartureMessageNotificationWithRequestId: OWrites[DepartureMessageNotification] =
    OWrites.transform(writesDepartureMessageNotification) {
      (departure, obj) =>
        obj ++ Json.obj("requestId" -> Constants.requestId(departure.departureId))
    }

  def fromRequest(request: DepartureResponseRequest[NodeSeq], timestamp: LocalDateTime): DepartureMessageNotification =
    DepartureMessageNotification(
      request.departure.departureId,
      request.departure.messages.length,
      timestamp,
      request.messageResponse.messageType,
      request.body
    )
}
