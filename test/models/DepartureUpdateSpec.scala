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

import java.time.Clock
import java.time.LocalDateTime
import java.time.ZoneOffset

import base.FreeSpecDiscipline
import base.SpecBase
import cats._
import cats.kernel.laws.discipline.SemigroupTests
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import java.time.LocalDate
import java.time.ZoneOffset
import java.time.Clock
import java.time.LocalTime

class DepartureUpdateSpec
    extends SpecBase
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with ModelGenerators
    with FreeSpecDiscipline
    with MongoDateTimeFormats {

  implicit val eqDepartureStatusUpdate: Eq[DepartureUpdate] = _ == _
  val localDate                                             = LocalDate.now()
  val localTime                                             = LocalTime.of(1, 1)
  val localDateTime                                         = LocalDateTime.of(localDate, localTime)
  implicit val clock                                        = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

  "DepartureUpdate" - {

    "combine" - {

      checkAll("Semigroup behaviour", SemigroupTests[DepartureUpdate].semigroup)

      "returns the value when it is combined with itself" in {
        forAll(arbitrary[DepartureUpdate]) {
          departureUpdate =>
            Semigroup[DepartureUpdate].combine(departureUpdate, departureUpdate) mustEqual departureUpdate
        }
      }

      "returns the value on the right when it is the same type of update" in {
        forAll(departureUpdateTypeGenerator) {
          departureUpdateupdateGenerator =>
            forAll(departureUpdateupdateGenerator, departureUpdateupdateGenerator) {
              (lhs, rhs) =>
                Semigroup[DepartureUpdate].combine(lhs, rhs) mustEqual rhs
            }
        }
      }
    }

    "DepartureModifier for any combination of DepartureUpdate is the same as the individual DepartureModifier combined" - {

      "when combined with an update that updates a message" in {
        def removeMessageUpdate(json: JsObject): JsObject = {
          val updateDescription = (json \ "$set").toOption.value
            .asInstanceOf[JsObject]
            .fields
            .filterNot(_._1.contains("messages."))

          Json.obj("$set" -> JsObject(updateDescription))
        }

        val updatesWithMessage = Gen.oneOf(
          arbitrary[MessageStatusUpdate],
          arbitrary[MessageStatusUpdate]
        )

        forAll(arbitrary[DepartureUpdate], updatesWithMessage) {
          (lhs, rhs: DepartureUpdate) =>
            val result = Semigroup[DepartureUpdate].combine(lhs, rhs)

            val expectedValue =
              removeMessageUpdate(DepartureModifier.toJson(lhs)) deepMerge DepartureModifier.toJson(rhs)

            DepartureModifier.toJson(result) mustEqual expectedValue

        }

      }
    }

  }

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
