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

import play.api.libs.json._
import utils.NodeSeqFormat
import utils.XmlToJson

import scala.xml.NodeSeq
import play.api.libs.functional.syntax._
import java.time.ZoneOffset
import java.time.OffsetDateTime

sealed trait Message {
  def dateTime: LocalDateTime
  def messageType: MessageType
  def message: NodeSeq
  def messageJson: JsObject
  def messageCorrelationId: Int
  def optStatus: Option[MessageStatus]

  def receivedBefore(requestedDate: OffsetDateTime): Boolean =
    dateTime.atOffset(ZoneOffset.UTC).isBefore(requestedDate)
}

final case class MessageWithStatus(dateTime: LocalDateTime,
                                   messageType: MessageType,
                                   message: NodeSeq,
                                   status: MessageStatus,
                                   messageCorrelationId: Int,
                                   messageJson: JsObject)
    extends Message { def optStatus = Some(status) }

final case class MessageWithoutStatus(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int, messageJson: JsObject)
    extends Message {
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

}

object MessageWithStatus extends NodeSeqFormat with MongoDateTimeFormats with XmlToJson {

  implicit val readsMessagWithStatus: Reads[MessageWithStatus] =
    (
      (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "status").read[MessageStatus] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MessageWithStatus(_, _, _, _, _, _))

  implicit val writesMessageWithStatus: OWrites[MessageWithStatus] =
    Json.writes[MessageWithStatus]

  def apply(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, status: MessageStatus, messageCorrelationId: Int): MessageWithStatus =
    MessageWithStatus(dateTime, messageType, message, status, messageCorrelationId, toJson(message))
}

object MessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats with XmlToJson {

  implicit val readsMessagWithoutStatus: Reads[MessageWithoutStatus] =
    (
      (__ \ "dateTime").read[LocalDateTime] and
        (__ \ "messageType").read[MessageType] and
        (__ \ "message").read[NodeSeq] and
        (__ \ "messageCorrelationId").read[Int] and
        (__ \ "messageJson").read[JsObject].orElse(Reads.pure(Json.obj()))
    )(MessageWithoutStatus(_, _, _, _, _))

  implicit val writesMessageWithoutStatus: OWrites[MessageWithoutStatus] =
    Json.writes[MessageWithoutStatus]

  def apply(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int): MessageWithoutStatus =
    MessageWithoutStatus(dateTime, messageType, message, messageCorrelationId, toJson(message))

}
