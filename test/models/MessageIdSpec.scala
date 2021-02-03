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

import generators.BaseGenerators
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.PathBindable

class MessageIdSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with BaseGenerators with OptionValues with EitherValues {

  "MessageId" - {

    val pathBindable = implicitly[PathBindable[MessageId]]

    "must bind from a URL for all message id values greater than zero and return the index of the message" in {
      forAll(intsAboveValue(0)) {
        value =>
          val expectedMessageIndex = value - 1

          val result = pathBindable.bind("key", value.toString).right.value

          result.index mustEqual expectedMessageIndex
      }
    }

    "must not bind from a URL for all all message id values less than 1" in {
      forAll(intsBelowValue(1)) {
        value =>
          pathBindable.bind("key", value.toString).left.value
      }
    }

    "must unbind and convert the index to the message id value" in {
      forAll(intsAboveValue(0)) {
        value =>
          val messageId            = MessageId.fromIndex(value)
          val expectedMessageIndex = (value + 1).toString

          pathBindable.unbind("key", messageId) mustEqual expectedMessageIndex
      }

    }
  }
}
