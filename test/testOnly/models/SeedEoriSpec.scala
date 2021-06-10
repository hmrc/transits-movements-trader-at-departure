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

package testOnly.models

import base.SpecBase
import play.api.libs.json.Json

class SeedEoriSpec extends SpecBase {

  "SeedEori" - {

    "json deserialization" - {
      "passes when the value is in the format: 2 alpha characters + 12 numeric characters" in {
        val json = Json.toJson("ZZ000000000001")

        json.as[SeedEori] mustEqual SeedEori("ZZ", 1, 12)
      }

      "fails when there is a non-numeric character in the last 12 characters" in {
        val json = Json.toJson("ZZ000X00000001")

        json.validate[SeedEori].isError mustEqual true
      }

      "fails when the string is not the correct length" in {
        val json = Json.toJson("ZZ001")

        json.validate[SeedEori].isError mustEqual true
      }

    }

    "must write from valid value in correct format" in {

      val json  = Json.toJson("ZZ000000000001")
      val json2 = Json.toJson("GB602070107000")

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori2 = SeedEori("GB", 602070107000L, 1)

      Json.toJson(seedEori) mustEqual json
      Json.toJson(seedEori2) mustEqual json2
    }

  }

}
