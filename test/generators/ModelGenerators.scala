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

package generators

import java.time.{LocalDate, LocalDateTime}

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import models.MessageStatus.SubmissionPending
import models.SubmissionProcessingResult.{SubmissionFailure, SubmissionFailureExternal, SubmissionFailureInternal}
import models.{Departure, DepartureId, DepartureStatus, MessageId, MessageType, MessageWithStatus, MessageWithoutStatus, MovementReferenceNumber, SubmissionProcessingResult}

trait ModelGenerators extends BaseGenerators with JavaTimeGenerators {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit lazy val arbitraryDepartureId: Arbitrary[DepartureId] = {
    Arbitrary {
      for {
        id <- intWithMaxLength(9)
      } yield DepartureId(id)
    }
  }

  implicit lazy val arbitraryMovementReferenceNumber: Arbitrary[MovementReferenceNumber] =
    Arbitrary {
      for {
        year    <- Gen.choose(0, 99).map(y => f"$y%02d")
        country <- Gen.pick(2, 'A' to 'Z')
        serial  <- Gen.pick(13, ('A' to 'Z') ++ ('0' to '9'))
      } yield MovementReferenceNumber(year ++ country.mkString ++ serial.mkString)
    }

  implicit lazy val arbitraryMessageId: Arbitrary[MessageId] =
    Arbitrary {
      intsAboveValue(0).map(MessageId.fromIndex)
    }

  implicit lazy val arbitraryMessageWithStateXml: Arbitrary[MessageWithStatus] = {
    Arbitrary {
      for {
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
        status = SubmissionPending
      } yield MessageWithStatus(LocalDateTime.of(date, time), messageType, xml, status, 1)
    }
  }

  implicit lazy val arbitraryMessageWithoutStateXml: Arbitrary[MessageWithoutStatus] = {
    Arbitrary {
      for {
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
      } yield MessageWithoutStatus(LocalDateTime.of(date, time), messageType, xml, 1)
    }
  }

  implicit lazy val arbitraryState: Arbitrary[DepartureStatus] =
    Arbitrary {
      Gen.oneOf(DepartureStatus.values)
    }

  implicit lazy val arbitraryDeparture: Arbitrary[Departure] =
    Arbitrary {
      for {
        id <- arbitrary[DepartureId]
        eN <- arbitrary[String]
        mrn <- arbitrary[MovementReferenceNumber]
        rN <- arbitrary[String]
        status <- arbitrary[DepartureStatus]
        created <- arbitrary[LocalDateTime]
        updated <- arbitrary[LocalDateTime]
        messages <- nonEmptyListOfMaxLength[MessageWithStatus](2)
      } yield models.Departure(id, eN, Some(mrn), rN, status, created, updated, messages.length + 1, messages)
    }

  implicit lazy val arbitraryFailure: Arbitrary[SubmissionFailure] =
    Arbitrary(Gen.oneOf(SubmissionFailureInternal, SubmissionFailureExternal))

  implicit lazy val arbitrarySubmissionResult: Arbitrary[SubmissionProcessingResult] =
    Arbitrary(Gen.oneOf(SubmissionProcessingResult.values))

  implicit lazy val arbitraryMessageType: Arbitrary[MessageType] =
    Arbitrary(Gen.oneOf(MessageType.values))

}
