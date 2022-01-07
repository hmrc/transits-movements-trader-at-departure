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

import java.time.LocalDateTime

import base.FreeSpecDiscipline
import base.SpecBase
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json
import java.time.LocalDate
import java.time.ZoneOffset
import java.time.Clock
import java.time.LocalTime

class MessageStatusUpdateSpec
    extends SpecBase
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with ModelGenerators
    with FreeSpecDiscipline
    with MongoDateTimeFormats {

  val localDate      = LocalDate.now()
  val localTime      = LocalTime.of(1, 1)
  val localDateTime  = LocalDateTime.of(localDate, localTime)
  implicit val clock = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

  "MessageStatusUpdate" - {
    "DepartureModifier returns modify object that would set the message status" in {
      forAll(arbitrary[MessageStatusUpdate]) {
        messageStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              s"messages.${messageStatusUpdate.messageId.index}.status" -> messageStatusUpdate.messageStatus,
              "lastUpdated"                                             -> LocalDateTime.now(clock)
            )
          )

          DepartureModifier.toJson(messageStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }
}
