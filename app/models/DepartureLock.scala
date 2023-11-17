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

import play.api.libs.functional.syntax._
import play.api.libs.json.Format
import play.api.libs.json.__

import java.time.LocalDateTime

case class DepartureLock(
  id: String,
  created: LocalDateTime
)

object DepartureLock {

  implicit val format: Format[DepartureLock] = {
    implicit val dtf: Format[LocalDateTime] = MongoDateTimeFormats.localDateTimeFormat
    ((__ \ "_id").format[String]
      ~ (__ \ "created").format[LocalDateTime])(DepartureLock.apply, unlift(DepartureLock.unapply))
  }

  val id      = "_id"
  val created = "created"

}
