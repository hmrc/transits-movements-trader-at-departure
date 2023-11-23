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

import models.MessageWithStatus.readsMessagWithStatus
import models.MessageWithoutStatus.readsMessagWithoutStatus
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.NodeSeqFormat

import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import scala.xml.NodeSeq

trait MessageTypeWithTime {
  def dateTime: LocalDateTime
  def messageType: MessageType
}

sealed trait Message extends MessageTypeWithTime {
  def messageId: MessageId
  def dateTime: LocalDateTime
  def received: Option[LocalDateTime]
  def messageType: MessageType
  def message: NodeSeq
  def messageCorrelationId: Int
  def optStatus: Option[MessageStatus]

  def receivedBefore(requestedDate: OffsetDateTime): Boolean =
    received.getOrElse(dateTime).atOffset(ZoneOffset.UTC).isBefore(requestedDate)
}

final case class MessageWithStatus(
  messageId: MessageId,
  dateTime: LocalDateTime,
  received: Option[LocalDateTime],
  messageType: MessageType,
  message: NodeSeq,
  status: MessageStatus,
  messageCorrelationId: Int
) extends Message { def optStatus = Some(status) }

final case class MessageWithoutStatus(
  messageId: MessageId,
  dateTime: LocalDateTime,
  received: Option[LocalDateTime],
  messageType: MessageType,
  message: NodeSeq,
  messageCorrelationId: Int
) extends Message {
  def optStatus = None
}

object Message extends NodeSeqFormat with MongoDateTimeFormats {

  implicit lazy val reads: Reads[Message] = new Reads[Message] {

    override def reads(json: JsValue): JsResult[Message] = (json \ "status").toOption match {
      case Some(_) =>
        json.validate[MessageWithStatus]
      case None =>
        json.validate[MessageWithoutStatus]
    }
  }

  implicit lazy val writes: OWrites[Message] = OWrites {
    case ns: MessageWithStatus    => Json.toJsObject(ns)(MessageWithStatus.writesMessageWithStatus)
    case ws: MessageWithoutStatus => Json.toJsObject(ws)(MessageWithoutStatus.writesMessageWithoutStatus)
  }
  implicit lazy val format: Format[Message] = Format(reads, writes)
}

object MessageWithStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val readsMessagWithStatus: Reads[MessageWithStatus] =
    (
      (__ \ "messageId").read[MessageId] and
        (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "received").readNullable[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "status").read[MessageStatus] and
        (__ \ "messageCorrelationId").read[Int]
    )(MessageWithStatus(_, _, _, _, _, _, _))

  implicit val writesMessageWithStatus: OWrites[MessageWithStatus] =
    Json.writes[MessageWithStatus]
  implicit lazy val format: OFormat[MessageWithStatus] = OFormat(readsMessagWithStatus, writesMessageWithStatus)
}

object MessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val readsMessagWithoutStatus: Reads[MessageWithoutStatus] =
    (
      (__ \ "messageId").read[MessageId] and
        (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "received").readNullable[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "messageCorrelationId").read[Int]
    )(MessageWithoutStatus(_, _, _, _, _, _))

  implicit val writesMessageWithoutStatus: OWrites[MessageWithoutStatus] =
    Json.writes[MessageWithoutStatus]
  implicit lazy val format: OFormat[MessageWithoutStatus] = OFormat(readsMessagWithoutStatus, writesMessageWithoutStatus)
}
