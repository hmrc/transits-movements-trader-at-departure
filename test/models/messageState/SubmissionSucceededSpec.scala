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

package models.messageState

import generators.ModelGenerators
import models.MessageStatus.SubmissionSucceeded
import models.SubmissionProcessingResult
import org.scalacheck.Arbitrary
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SubmissionSucceededSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with ModelGenerators {

  "SubmissionSucceeded must transition" - {

    "to SubmissionSuceeded when receiving any Submission event" in {
      forAll(Arbitrary.arbitrary[SubmissionProcessingResult]) {
        submissionResult =>
          SubmissionSucceeded.transition(submissionResult) mustEqual SubmissionSucceeded
      }
    }
  }
}
