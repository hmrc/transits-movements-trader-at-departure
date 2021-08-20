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

import base.SpecBase
import generators.ModelGenerators
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SubmissionProcessingResultSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "SubmissionProcessingResult.values must contain" - {
    "SubmissionSuccess" in {
      SubmissionProcessingResult.values must contain(SubmissionSuccess)
    }

    "SubmissionFailureInternal" in {
      SubmissionProcessingResult.values must contain(SubmissionFailureInternal)
    }

    "SubmissionFailureExternal" in {
      SubmissionProcessingResult.values must contain(SubmissionFailureExternal)
    }
  }
}
