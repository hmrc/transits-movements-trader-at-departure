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

package models

import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsString
import play.api.libs.json.Json

class MovementReferenceNumberSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ModelGenerators with EitherValues with OptionValues {

  "a Movement Reference Number" - {

    "must deserialise" in {

      forAll(arbitrary[MovementReferenceNumber]) {
        mrn =>
          JsString(mrn.value).as[MovementReferenceNumber] mustEqual mrn
      }
    }

    "must serialise" in {

      forAll(arbitrary[MovementReferenceNumber]) {
        mrn =>
          Json.toJson(mrn) mustEqual JsString(mrn.value)
      }
    }
  }
}
