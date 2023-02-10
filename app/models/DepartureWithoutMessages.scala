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

import cats.data.NonEmptyList
import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.time.LocalDateTime

case class DepartureWithoutMessages(
  departureId: DepartureId,
  channel: ChannelType,
  eoriNumber: String,
  movementReferenceNumber: Option[MovementReferenceNumber],
  referenceNumber: String,
  created: LocalDateTime,
  lastUpdated: LocalDateTime,
  notificationBox: Option[Box],
  nextMessageId: MessageId,
  nextMessageCorrelationId: Int,
  messagesMetaData: Seq[MessageMetaData]
) extends BaseDeparture {

  override val messagesList: List[MessageTypeWithTime] = messagesMetaData.toList
}

object DepartureWithoutMessages {

  implicit def formatsNonEmptyList[A](implicit listReads: Reads[List[A]], listWrites: Writes[List[A]]): Format[NonEmptyList[A]] =
    new Format[NonEmptyList[A]] {
      override def writes(o: NonEmptyList[A]): JsValue = Json.toJson(o.toList)

      override def reads(json: JsValue): JsResult[NonEmptyList[A]] = json.validate(listReads).map(NonEmptyList.fromListUnsafe)
    }

  implicit val readsDeparture: Reads[DepartureWithoutMessages] =
    (
      (__ \ "_id").read[DepartureId] and
        (__ \ "channel").read[ChannelType] and
        (__ \ "eoriNumber").read[String] and
        (__ \ "movementReferenceNumber").readNullable[MovementReferenceNumber] and
        (__ \ "referenceNumber").read[String] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "lastUpdated")
          .read(MongoDateTimeFormats.localDateTimeRead)
          .orElse((__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead)) and
        (__ \ "notificationBox").readNullable[Box] and
        (__ \ "nextMessageId").read[MessageId] and
        (__ \ "nextMessageCorrelationId").read[Int] and
        (__ \ "messages").read[Seq[MessageMetaData]]
    )(DepartureWithoutMessages.apply _)

  val projection: JsObject = Json.obj(
    "_id"                      -> 1,
    "channel"                  -> 1,
    "eoriNumber"               -> 1,
    "movementReferenceNumber"  -> 1,
    "referenceNumber"          -> 1,
    "created"                  -> 1,
    "updated"                  -> 1,
    "lastUpdated"              -> 1,
    "notificationBox"          -> 1,
    "nextMessageCorrelationId" -> 1,
    "messages"                 -> 1
  )
}
