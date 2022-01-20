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

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}

class DepartureSelectorSpec extends AnyFreeSpec with Matchers with ScalaFutures {

  "DepartureSelector" - {

    "must serialise from DepartureIdSelector" in {

      val departureIdSelector: DepartureSelector = DepartureIdSelector(DepartureId(1))

      val expectedResult = JsObject(Map("_id" -> JsNumber(1)))

      Json.toJsObject(departureIdSelector) mustBe expectedResult
    }

    "must serialise from MessageSelector" in {

      val messageSelector: DepartureSelector = MessageSelector(DepartureId(1), MessageId(1))

      val expectedResult = JsObject(
        Map(
          "$and" ->
            JsArray(
              Seq(
                Json.obj("_id" -> 1),
                Json.obj("messages.0.status" -> Json.obj("$exists" -> true))
              )
            )
        )
      )

      Json.toJsObject(messageSelector) mustBe expectedResult
    }
  }
}
