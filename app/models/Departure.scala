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

import cats.data._
import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.time.LocalDateTime

trait BaseDeparture {
  def departureId: DepartureId

  def channel: ChannelType

  def eoriNumber: String

  def movementReferenceNumber: Option[MovementReferenceNumber]

  def referenceNumber: String

  def status: DepartureStatus

  def created: LocalDateTime

  def lastUpdated: LocalDateTime

  def notificationBox: Option[Box]
}

case class Departure(
  departureId: DepartureId,
  channel: ChannelType,
  eoriNumber: String,
  movementReferenceNumber: Option[MovementReferenceNumber],
  referenceNumber: String,
  status: DepartureStatus,
  created: LocalDateTime,
  lastUpdated: LocalDateTime,
  nextMessageCorrelationId: Int,
  messages: NonEmptyList[Message],
  notificationBox: Option[Box]
) extends BaseDeparture {

  def nextMessageId: MessageId = MessageId(messages.length + 1)

  def messagesWithId: NonEmptyList[(Message, MessageId)] =
    messages.map(msg => msg -> msg.messageId)
}

object Departure {

  implicit def formatsNonEmptyList[A](implicit listReads: Reads[List[A]], listWrites: Writes[List[A]]): Format[NonEmptyList[A]] =
    new Format[NonEmptyList[A]] {
      override def writes(o: NonEmptyList[A]): JsValue = Json.toJson(o.toList)

      override def reads(json: JsValue): JsResult[NonEmptyList[A]] = json.validate(listReads).map(NonEmptyList.fromListUnsafe)
    }

  implicit val readsDeparture: Reads[Departure] =
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
        (__ \ "nextMessageCorrelationId").read[Int] and
        (__ \ "messages").read[NonEmptyList[Message]] and
        (__ \ "notificationBox").readNullable[Box]
    )(Departure.apply _)

  implicit def writesDeparture(implicit write: Writes[LocalDateTime]): OWrites[Departure] =
    (
      (__ \ "_id").write[DepartureId] and
        (__ \ "channel").write[ChannelType] and
        (__ \ "eoriNumber").write[String] and
        (__ \ "movementReferenceNumber").writeNullable[MovementReferenceNumber] and
        (__ \ "referenceNumber").write[String] and
        (__ \ "status").write[DepartureStatus] and
        (__ \ "created").write(write) and
        (__ \ "lastUpdated").write(write) and
        (__ \ "nextMessageCorrelationId").write[Int] and
        (__ \ "messages").write[NonEmptyList[Message]] and
        (__ \ "notificationBox").writeNullable[Box]
    )(unlift(Departure.unapply))

  implicit def formatsDeparture(implicit write: Writes[LocalDateTime]): OFormat[Departure] =
    OFormat(readsDeparture, writesDeparture)
}
