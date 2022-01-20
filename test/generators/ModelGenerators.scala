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

package generators

import config.Constants
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult.DownstreamInternalServerError
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionFailure
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionFailureDownstream
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionRejected
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import connectors.MessageConnector.EisSubmissionResult.UnexpectedHttpResponse
import connectors.MessageConnector.EisSubmissionResult.VirusFoundOrInvalidToken
import models.MessageStatus.SubmissionPending
import models.SubmissionProcessingResult.SubmissionFailure
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import uk.gov.hmrc.http.UpstreamErrorResponse
import utils.JsonHelper

import java.time.LocalDate
import java.time.LocalDateTime

trait ModelGenerators extends BaseGenerators with JavaTimeGenerators with JsonHelper {

  private val pastDate: LocalDate = LocalDate.of(1900, 1, 1)
  private val dateNow: LocalDate  = LocalDate.now

  implicit val arbitraryMessageStatusUpdate: Arbitrary[MessageStatusUpdate] =
    Arbitrary {
      for {
        messageId     <- arbitrary[MessageId]
        messageStatus <- arbitrary[MessageStatus]
      } yield MessageStatusUpdate(messageId, messageStatus)
    }

  implicit lazy val arbitraryMessageStatus: Arbitrary[MessageStatus] =
    Arbitrary {
      Gen.oneOf(MessageStatus.values)
    }

  implicit lazy val arbitraryDepartureId: Arbitrary[DepartureId] =
    Arbitrary {
      for {
        id <- intWithMaxLength(9)
      } yield DepartureId(id)
    }

  implicit lazy val arbitraryMovementReferenceNumber: Arbitrary[MovementReferenceNumber] =
    Arbitrary {
      for {
        year <- Gen
          .choose(0, 99)
          .map(
            y => f"$y%02d"
          )
        country <- Gen.pick(2, 'A' to 'Z')
        serial  <- Gen.pick(13, ('A' to 'Z') ++ ('0' to '9'))
      } yield MovementReferenceNumber(year ++ country.mkString ++ serial.mkString)
    }

  implicit lazy val arbitraryMessageId: Arbitrary[MessageId] =
    Arbitrary {
      intsAboveValue(0).map(MessageId.apply)
    }

  implicit lazy val arbitraryMessageWithStateXml: Arbitrary[MessageWithStatus] =
    Arbitrary {
      for {
        messageId   <- arbitrary[MessageId]
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
        status = SubmissionPending
      } yield MessageWithStatus(messageId, LocalDateTime.of(date, time), messageType, xml, status, 1, convertXmlToJson(xml.toString()))
    }

  implicit lazy val arbitraryMessageWithoutStateXml: Arbitrary[MessageWithoutStatus] =
    Arbitrary {
      for {
        messageId   <- arbitrary[MessageId]
        date        <- datesBetween(pastDate, dateNow)
        time        <- timesBetween(pastDate, dateNow)
        xml         <- Gen.const(<blankXml>message</blankXml>)
        messageType <- Gen.oneOf(MessageType.values)
      } yield MessageWithoutStatus(messageId, LocalDateTime.of(date, time), messageType, xml, 1, convertXmlToJson(xml.toString()))
    }

  implicit lazy val arbitraryState: Arbitrary[DepartureStatus] =
    Arbitrary {
      Gen.oneOf(DepartureStatus.values)
    }

  implicit lazy val arbitraryChannel: Arbitrary[ChannelType] =
    Arbitrary {
      Gen.oneOf(ChannelType.values)
    }

  implicit lazy val arbitraryBoxId: Arbitrary[BoxId] =
    Arbitrary {
      Gen.uuid.map(_.toString).map(BoxId.apply)
    }

  implicit lazy val arbitraryBox: Arbitrary[Box] =
    Arbitrary {
      for {
        boxId <- arbitrary[BoxId]
      } yield Box(boxId, Constants.BoxName)
    }

  implicit lazy val arbitraryDeparture: Arbitrary[Departure] =
    Arbitrary {
      for {
        id              <- arbitrary[DepartureId]
        channel         <- arbitrary[ChannelType]
        eN              <- arbitrary[String]
        mrn             <- arbitrary[MovementReferenceNumber]
        rN              <- arbitrary[String]
        created         <- arbitrary[LocalDateTime]
        lastUpdated     <- arbitrary[LocalDateTime]
        messages        <- nonEmptyListOfMaxLength[MessageWithStatus](2)
        notificationBox <- arbitrary[Option[Box]]
      } yield models.Departure(id, channel, eN, Some(mrn), rN, created, lastUpdated, messages.length + 1, messages, notificationBox)
    }

  implicit lazy val arbitraryDepartureWithoutMessages: Arbitrary[DepartureWithoutMessages] =
    Arbitrary {
      arbitraryDeparture.arbitrary.map {
        dep =>
          DepartureWithoutMessages.fromDeparture(dep)
      }
    }

  implicit lazy val arbitraryFailure: Arbitrary[SubmissionFailure] =
    Arbitrary(Gen.oneOf(SubmissionFailureInternal, SubmissionFailureExternal))

  implicit lazy val arbitrarySubmissionResult: Arbitrary[SubmissionProcessingResult] =
    Arbitrary(Gen.oneOf(SubmissionProcessingResult.values))

  implicit lazy val arbitraryMessageType: Arbitrary[MessageType] =
    Arbitrary(Gen.oneOf(MessageType.values))

  implicit lazy val arbitrarySubmissionFailure: Arbitrary[EisSubmissionFailure] =
    Arbitrary(Gen.oneOf(arbitrary[EisSubmissionRejected], arbitrary[EisSubmissionFailureDownstream]))

  implicit lazy val arbitrarySubmissionFailureInternal: Arbitrary[EisSubmissionRejected] =
    Arbitrary {
      Gen.oneOf(
        ErrorInPayload,
        VirusFoundOrInvalidToken
      )
    }

  implicit lazy val arbitrarySubmissionFailureDownstream: Arbitrary[EisSubmissionFailureDownstream] =
    Arbitrary {
      Gen.oneOf(
        DownstreamInternalServerError,
        UnexpectedHttpResponse(UpstreamErrorResponse("", 418))
      )
    }

  implicit def arbitraryEisSubmissionResult: Arbitrary[EisSubmissionResult] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[EisSubmissionRejected],
        arbitrary[EisSubmissionFailureDownstream],
        Gen.const(EisSubmissionSuccessful)
      )
    )

  implicit lazy val arbitraryDepartureMessage: Arbitrary[DepartureMessages] =
    Arbitrary {
      for {
        departureId <- arbitrary[DepartureId]
        eoriNumber  <- arbitrary[String]
        message     <- arbitrary[MessageWithStatus]
      } yield DepartureMessages(departureId, EORINumber(eoriNumber), List(message))
    }
}
