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

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ChannelType.Web
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDateTime

class SubmissionProcessingResultSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "SubmissionProcessingResult.values must contain" - {
    "SubmissionSuccess" in {
      SubmissionProcessingResult.values must contain(SubmissionProcessingResult.SubmissionSuccess)
    }

    "SubmissionFailureInternal" in {
      SubmissionProcessingResult.values must contain(SubmissionProcessingResult.SubmissionFailureInternal)
    }

    "SubmissionFailureExternal" in {
      SubmissionProcessingResult.values must contain(SubmissionProcessingResult.SubmissionFailureExternal)
    }
  }
  "toEither" - {
    val departure: Departure = Departure(
      departureId = DepartureId(1),
      channel = Web,
      movementReferenceNumber = None,
      referenceNumber = "SomeREf",
      eoriNumber = "AB123456",
      created = LocalDateTime.of(2021, 2, 2, 2, 2),
      lastUpdated = LocalDateTime.of(2021, 2, 2, 4, 2),
      messages = NonEmptyList.one(
        MessageWithStatus(
          MessageId(1),
          LocalDateTime.of(2021, 2, 2, 2, 2),
          Some(LocalDateTime.of(2021, 2, 2, 2, 2)),
          MessageType.DepartureDeclaration,
          <CC015></CC015>,
          MessageStatus.SubmissionPending,
          1
        )
      ),
      nextMessageCorrelationId = 2,
      notificationBox = None
    )

    "must convert from SubmissionProcessingResult.SubmissionFailureInternal to Left(SubmissionFailureInternal)" in {
      SubmissionProcessingResult.SubmissionFailureInternal.toEither(departure) mustBe Left(SubmissionFailureInternal)
    }

    "must convert from SubmissionProcessingResult.SubmissionFailureExternal to Left(SubmissionFailureExternal)" in {
      SubmissionProcessingResult.SubmissionFailureExternal.toEither(departure) mustBe Left(SubmissionFailureExternal)
    }

    "must convert from SubmissionProcessingResult.SubmissionSuccess to Right(SubmissionSuccess)" in {
      SubmissionProcessingResult.SubmissionSuccess.toEither(departure) mustBe Right(SubmissionSuccess(departure))
    }

    "must convert from SubmissionProcessingResult.SubmissionFailureRejected to Left(SubmissionFailureRejected)" in {
      SubmissionProcessingResult.SubmissionFailureRejected("Woops").toEither(departure) mustBe Left(SubmissionFailureRejected("Woops"))
    }
  }
}
