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

import java.time.LocalDateTime

import cats.data.NonEmptyList
import play.api.libs.json.Format
import play.api.libs.json.JsObject
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.__
import play.api.libs.functional.syntax._

case class DepartureWithoutMessages(
  departureId: DepartureId,
  channel: ChannelType,
  eoriNumber: String,
  movementReferenceNumber: Option[MovementReferenceNumber],
  referenceNumber: String,
  status: DepartureStatus,
  created: LocalDateTime,
  lastUpdated: LocalDateTime,
  notificationBox: Option[Box],
  nextMessageId: MessageId,
  nextMessageCorrelationId: Int,
  messagesMetaData: Seq[MessageMetaData]
) extends BaseDeparture {}

object DepartureWithoutMessages {

  def fromDeparture(departure: Departure) =
    DepartureWithoutMessages(
      departure.departureId,
      departure.channel,
      departure.eoriNumber,
      departure.movementReferenceNumber,
      departure.referenceNumber,
      departure.status,
      departure.created,
      departure.lastUpdated,
      departure.notificationBox,
      departure.nextMessageId,
      departure.nextMessageCorrelationId,
      departure.messages.map(x => MessageMetaData(x.messageType, x.dateTime)).toList
    )

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
        (__ \ "status").read[DepartureStatus] and
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
    "status"                   -> 1,
    "created"                  -> 1,
    "updated"                  -> 1,
    "lastUpdated"              -> 1,
    "notificationBox"          -> 1,
    "nextMessageCorrelationId" -> 1,
    "messages"                 -> 1
  )
}
