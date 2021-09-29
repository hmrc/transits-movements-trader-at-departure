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

import play.api.libs.json.Json
import play.api.libs.json.OFormat

import java.time.LocalDateTime

case class LatestMessages(current: MessageMetaData, previous: MessageMetaData)

object LatestMessages {

  implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

  def fromMessageMetaData(messages: Seq[MessageMetaData]): LatestMessages = {
    val latestMessages = messages.sortBy(_.dateTime)(localDateOrdering.reverse).take(2)

    LatestMessages(latestMessages.head, latestMessages(1))
  }

  def fromMessages(messages: Seq[Message]): LatestMessages = {
    val latestMessage = messages
      .sortBy(_.dateTime)(localDateOrdering.reverse)
      .take(2)
      .map(x => MessageMetaData(x.messageType, x.dateTime))

    LatestMessages(latestMessage.head, latestMessage(1))
  }

  implicit val format: OFormat[LatestMessages] = Json.format[LatestMessages]
}
