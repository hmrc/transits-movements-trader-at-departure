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

import java.time.Instant
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset

import play.api.libs.json._

trait MongoDateTimeFormats {

  implicit val localDateTimeRead: Reads[LocalDateTime] =
    (__ \ "$date").read[Long].map {
      millis =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
    }

  implicit val localDateTimeWrite: Writes[LocalDateTime] = new Writes[LocalDateTime] {

    def writes(dateTime: LocalDateTime): JsValue = Json.obj(
      "$date" -> dateTime.atZone(ZoneOffset.UTC).toInstant.toEpochMilli
    )
  }

  implicit val localDateTimeFormat: Format[LocalDateTime] =
    Format(localDateTimeRead, localDateTimeWrite)

  implicit val offsetDateTimeRead: Reads[OffsetDateTime] =
    localDateTimeRead.map(_.atOffset(ZoneOffset.UTC))

  implicit val offsetDateTimeWrite: Writes[OffsetDateTime] =
    localDateTimeWrite.contramap(_.atZoneSameInstant(ZoneOffset.UTC).toLocalDateTime)
}

object MongoDateTimeFormats extends MongoDateTimeFormats
