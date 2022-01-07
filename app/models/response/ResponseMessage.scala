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

package models.response

import java.time.LocalDateTime

import controllers.routes
import models.DepartureId
import models.Message
import play.api.libs.json.Json
import play.api.libs.json.OWrites
import utils.NodeSeqFormat

import scala.xml.NodeSeq

case class ResponseMessage(location: String, dateTime: LocalDateTime, messageType: String, message: NodeSeq)

object ResponseMessage extends NodeSeqFormat {

  def build(d: DepartureId, m: Message) =
    ResponseMessage(routes.MessagesController.getMessage(d, m.messageId).url, m.dateTime, m.messageType.code, m.message)

  implicit val writes: OWrites[ResponseMessage] = Json.writes[ResponseMessage]
}
