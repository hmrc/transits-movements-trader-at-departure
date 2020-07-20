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

package controllers

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageId
import models.MessageType
import models.MessageWithStatus
import models.MessageWithoutStatus
import models.MovementReferenceNumber
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.GET
import play.api.test.Helpers.contentAsJson
import play.api.test.Helpers.route
import play.api.test.Helpers.running
import repositories.DepartureRepository
import utils.Format
import play.api.test.Helpers._

import scala.concurrent.Future

class MessagesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  implicit val responseDepartureWrite = ResponseDepartureWithMessages.writes
  implicit val responseMessageWrite   = ResponseMessage.writes

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val mrn = arbitrary[MovementReferenceNumber].sample.value

  val requestXmlBody =
    <CC028A>
        <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
        <HEAHEA>
          <DocNumHEA5>{mrn.value}</DocNumHEA5>
        </HEAHEA>
      </CC028A>

  val message = MessageWithStatus(
    localDateTime,
    MessageType.MrnAllocated,
    requestXmlBody,
    SubmissionPending,
    2
  )

  val messageId = MessageId.fromIndex(0)

  val departureId = arbitrary[DepartureId].sample.value

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
  } yield {
    departure.copy(
      departureId,
      "eori",
      Some(mrn),
      "ref",
      DepartureStatus.MrnAllocated,
      localDateTime,
      localDateTime,
      message.messageCorrelationId,
    )
  }

  val departure = departureWithOneMessage.sample.value

  "getMessages" - {

    "must return OK" - {
      "with the retrieved messages" in {

        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          lazy val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
          val result       = route(application, request).value

          status(result) mustEqual OK

          val expectedMessages  = ResponseMessage.build(departure.departureId, MessageId.fromMessageIdValue(1).value, message)
          val expectedDeparture = ResponseDepartureWithMessages.build(departure).copy(messages = Seq(expectedMessages))

          println(expectedDeparture.location)

          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }

      "with only messages that are successful" in {
        val message1  = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val message2  = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

        val expectedMessages  = ResponseMessage.build(departure.departureId, MessageId.fromMessageIdValue(1).value, message1)
        val expectedDeparture = ResponseDepartureWithMessages.build(departure).copy(messages = Seq(expectedMessages))

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }

      }

      "with only messages that are successful and stateless" in {
        val message1 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val message2 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)
        val message3 = Arbitrary.arbitrary[MessageWithoutStatus].sample.value

        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2, message3), eoriNumber = "eori")

        val expectedMessage1  = ResponseMessage.build(departure.departureId, MessageId.fromMessageIdValue(1).value, message1)
        val expectedMessage3  = ResponseMessage.build(departure.departureId, MessageId.fromMessageIdValue(3).value, message3)
        val expectedDeparture = ResponseDepartureWithMessages.build(departure).copy(messages = Seq(expectedMessage1, expectedMessage3))

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }

      "with no messages if they are all failures" in {
        val message1 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)
        val message2 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)

        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

        val expectedDeparture = ResponseDepartureWithMessages.build(departure).copy(messages = Nil)

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
          val result  = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }
    }

    "must return NOT FOUND" - {
      "when departure is not found" in {
        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(DepartureId(1)).url)
          val result  = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }

      "when departure is inaccessible to the user" in {
        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message), eoriNumber = "eori2")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
          val result  = route(application, request).value

          status(result) mustEqual NOT_FOUND
        }
      }
    }

  }

}
