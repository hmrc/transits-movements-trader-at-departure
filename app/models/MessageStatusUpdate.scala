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

import play.api.libs.json.Json
import play.api.libs.json.Writes

import java.time.Clock
import java.time.LocalDateTime

final case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus)

object MessageStatusUpdate extends MongoDateTimeFormats {

  implicit def departureStateUpdate(implicit clock: Clock, writes: Writes[MessageStatus]): DepartureModifier[MessageStatusUpdate] =
    DepartureModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus,
              "lastUpdated"                               -> Json.toJson(LocalDateTime.now(clock))
            )
      )
    )
}
