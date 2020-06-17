/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.transitsmovementstraderatdeparture.models

import java.time.LocalDateTime

import play.api.libs.json.{JsResult, JsValue, Json, OFormat, OWrites, Reads}
import uk.gov.hmrc.transitsmovementstraderatdeparture.utils.NodeSeqFormat

import scala.xml.NodeSeq

sealed trait Message {
  def dateTime: LocalDateTime
  def messageType: MessageType
  def message: NodeSeq
  def optStatus: Option[MessageStatus]
}

final case class MessageWithStatus(dateTime: LocalDateTime,
                                           messageType: MessageType,
                                           message: NodeSeq,
                                           status: MessageStatus,
                                           messageCorrelationId: Int)
  extends Message { def optStatus = Some(status) }

final case class MessageWithoutStatus(dateTime: LocalDateTime, messageType: MessageType, message: NodeSeq, messageCorrelationId: Int)
  extends Message { def optStatus = None }

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
    case ns: MessageWithStatus    => Json.toJsObject(ns)(MessageWithStatus.formatsMessage)
    case ws: MessageWithoutStatus => Json.toJsObject(ws)(MessageWithoutStatus.formatsMessage)
  }

}

object MessageWithStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val formatsMessage: OFormat[MessageWithStatus] =
    Json.format[MessageWithStatus]
}

object MessageWithoutStatus extends NodeSeqFormat with MongoDateTimeFormats {

  implicit val formatsMessage: OFormat[MessageWithoutStatus] =
    Json.format[MessageWithoutStatus]
}