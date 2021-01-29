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

import cats.data._
import cats.implicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Departure(departureId: DepartureId,
                     channel: ChannelType,
                     eoriNumber: String,
                     movementReferenceNumber: Option[MovementReferenceNumber],
                     referenceNumber: String,
                     status: DepartureStatus,
                     created: LocalDateTime,
                     updated: LocalDateTime,
                     nextMessageCorrelationId: Int,
                     messages: NonEmptyList[Message]) {

  def nextMessageId: MessageId = MessageId.fromIndex(messages.length)

  def messagesWithId: NonEmptyList[(Message, MessageId)] =
    messages.mapWithIndex(_ -> MessageId.fromIndex(_))

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
        (__ \ "movementReferenceNumber").read[Option[MovementReferenceNumber]] and
        (__ \ "referenceNumber").read[String] and
        (__ \ "status").read[DepartureStatus] and
        (__ \ "created").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "updated").read(MongoDateTimeFormats.localDateTimeRead) and
        (__ \ "nextMessageCorrelationId").read[Int] and
        (__ \ "messages").read[NonEmptyList[Message]]
    )(Departure.apply _)

  implicit def writesDeparture(implicit write: Writes[LocalDateTime]): OWrites[Departure] =
    (
      (__ \ "_id").write[DepartureId] and
        (__ \ "channel").write[ChannelType] and
        (__ \ "eoriNumber").write[String] and
        (__ \ "movementReferenceNumber").write[Option[MovementReferenceNumber]] and
        (__ \ "referenceNumber").write[String] and
        (__ \ "status").write[DepartureStatus] and
        (__ \ "created").write(write) and
        (__ \ "updated").write(write) and
        (__ \ "nextMessageCorrelationId").write[Int] and
        (__ \ "messages").write[NonEmptyList[Message]]
    )(unlift(Departure.unapply))

}
