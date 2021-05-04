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

class DepartureUpdateSpec
    extends SpecBase
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with ModelGenerators
    with FreeSpecDiscipline
    with MongoDateTimeFormats {

  implicit val eqDepartureStatusUpdate: Eq[DepartureUpdate] = _ == _
  val currentDateTime                                       = LocalDateTime.now.withSecond(0).withNano(0).toInstant(ZoneOffset.UTC)
  implicit val clock: Clock                                 = Clock.fixed(currentDateTime, ZoneOffset.UTC)

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
      "when combined with an DepartureStatusUpdate" in {
        forAll(arbitrary[DepartureUpdate], arbitrary[DepartureStatusUpdate]) {
          (lhs, rhs) =>
            val result = Semigroup[DepartureUpdate].combine(lhs, rhs)

            val expectedValue = DepartureModifier.toJson(lhs) deepMerge DepartureModifier.toJson(rhs)

            DepartureModifier.toJson(result) mustEqual expectedValue

        }
      }

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
          arbitrary[CompoundStatusUpdate]
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
              "lastUpdated"                                             -> LocalDateTime.now.withSecond(0).withNano(0)
            )
          )

          DepartureModifier.toJson(messageStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }

  "DepartureStatusUpdate" - {
    "DepartureModifier returns modify object that would set the status" in {
      forAll(arbitrary[DepartureStatusUpdate]) {
        departureStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              "status"      -> departureStatusUpdate.departureStatus,
              "lastUpdated" -> LocalDateTime.now.withSecond(0).withNano(0)
            )
          )

          DepartureModifier.toJson(departureStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }

  "CompoundStatusUpdate" - {
    "DepartureModifier returns modify object that would set the status and the message status" in {
      forAll(arbitrary[CompoundStatusUpdate]) {
        compoundStatusUpdate =>
          val expectedUpdateJson = Json.obj(
            "$set" -> Json.obj(
              "status"                                                                       -> compoundStatusUpdate.departureStatusUpdate.departureStatus,
              s"messages.${compoundStatusUpdate.messageStatusUpdate.messageId.index}.status" -> compoundStatusUpdate.messageStatusUpdate.messageStatus,
              "lastUpdated"                                                                  -> LocalDateTime.now.withSecond(0).withNano(0)
            )
          )

          DepartureModifier.toJson(compoundStatusUpdate) mustEqual expectedUpdateJson
      }
    }
  }
}
