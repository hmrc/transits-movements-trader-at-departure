/*
 * Copyright 2023 HM Revenue & Customs
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

import audit.AuditService
import audit.AuditType._
import base.SpecBase
import cats.data.Ior
import cats.data.NonEmptyList
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import generators.ModelGenerators
import models.ChannelType.Web
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionSucceeded
import models._
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
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
import play.api.test.Helpers._
import repositories.DepartureRepository
import repositories.LockRepository
import services.SubmitMessageService
import utils.Format
import utils.JsonHelper

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import scala.concurrent.Future

class MessagesControllerSpec
    extends SpecBase
    with JsonHelper
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with BeforeAndAfterEach
    with IntegrationPatience {

  val departureWithoutMessages = arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori")

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val mrn = arbitrary[MovementReferenceNumber].sample.value

  val declarationCancellationRequestXmlBody =
    <CC014A>
      <SynVerNumMES2>123</SynVerNumMES2>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC014A>

  val savedDeclarationCancellationRequestXml =
    <CC014A>
      <SynVerNumMES2>123</SynVerNumMES2>
      <MesSenMES3>{MessageSender(departureWithoutMessages.departureId, 2).toString}</MesSenMES3>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC014A>

  val savedDeclarationCancellationRequestJsonBody = convertXmlToJson(savedDeclarationCancellationRequestXml.toString)

  val departureDeclarationRequestXmlBody =
    <CC015A>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>{mrn.value}</DocNumHEA5>
      </HEAHEA>
    </CC015A>

  "post" - {

    "must return Accepted, add the message to the departure, send the message upstream and set the message state to SubmissionSucceeded" in {

      val mockDepartureRepository  = mock[DepartureRepository]
      val mockLockRepository       = mock[LockRepository]
      val mockSubmitMessageService = mock[SubmitMessageService]
      val mockAuditService         = mock[AuditService]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))

      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository),
          bind[SubmitMessageService].toInstance(mockSubmitMessageService),
          bind[AuditService].toInstance(mockAuditService)
        )
        .build()

      running(application) {

        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual ACCEPTED

        header("Location", result).value must be(
          routes.MessagesController.getMessage(departureWithoutMessages.departureId, MessageId(departureWithoutMessages.nextMessageCorrelationId)).url
        )

        verify(mockSubmitMessageService, times(1)).submitMessage(
          eqTo(departureWithoutMessages.departureId),
          any(),
          any()
        )(any())

        verify(mockAuditService, times(1)).auditEvent(
          eqTo(DepartureCancellationRequestSubmitted),
          EnrolmentId(eqTo(Ior.right(EORINumber("eori")))),
          any(),
          any()
        )(any())
      }
    }

    "must return NotFound if there is no Departure for that DepartureId" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]
      val mockLockRepository      = mock[LockRepository]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any())).thenReturn(Future.successful(None))

      val application = baseApplicationBuilder
        .overrides(
          bind[LockRepository].toInstance(mockLockRepository),
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.MessagesController.post(DepartureId(1)).url)
          .withHeaders("channel" -> Web.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual NOT_FOUND
      }
    }

    "must return InternalServerError if there was an internal failure when saving and sending" in {
      val mockDepartureRepository  = mock[DepartureRepository]
      val mockLockRepository       = mock[LockRepository]
      val mockSubmitMessageService = mock[SubmitMessageService]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository),
          bind[SubmitMessageService].toInstance(mockSubmitMessageService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }

    "must return BadRequest if the payload is malformed" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockLockRepository      = mock[LockRepository]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      val application =
        baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[DepartureRepository].toInstance(mockDepartureRepository)
          )
          .build()

      running(application) {
        val requestXmlBody = <CC014A><HEAHEA></HEAHEA></CC014A>

        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(requestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustEqual "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
        status(result) mustEqual BAD_REQUEST
      }
    }

    "must return NotImplemented if the message is not supported" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]
      val mockLockRepository      = mock[LockRepository]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      val application =
        baseApplicationBuilder
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[MessageConnector].toInstance(mockMessageConnector)
          )
          .build()

      running(application) {
        val requestXmlBody = <CC099A><HEAHEA></HEAHEA></CC099A>

        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(requestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual NOT_IMPLEMENTED
      }
    }

    "must return BadGateway if there was an external failure when saving and sending" in {
      val mockDepartureRepository  = mock[DepartureRepository]
      val mockLockRepository       = mock[LockRepository]
      val mockSubmitMessageService = mock[SubmitMessageService]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository),
          bind[SubmitMessageService].toInstance(mockSubmitMessageService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual BAD_GATEWAY
      }
    }

    "must return BadRequest if there has been a rejection from EIS dues to schema validation failing" in {
      val mockDepartureRepository  = mock[DepartureRepository]
      val mockLockRepository       = mock[LockRepository]
      val mockSubmitMessageService = mock[SubmitMessageService]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureRejected(ErrorInPayload.responseBody)))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository),
          bind[SubmitMessageService].toInstance(mockSubmitMessageService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe "Message failed schema validation"
        status(result) mustEqual BAD_REQUEST
      }
    }

    "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockDepartureRepository  = mock[DepartureRepository]
      val mockLockRepository       = mock[LockRepository]
      val mockSubmitMessageService = mock[SubmitMessageService]

      when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
      when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departureWithoutMessages)))

      when(mockSubmitMessageService.submitMessage(any(), any(), any())(any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository),
          bind[SubmitMessageService].toInstance(mockSubmitMessageService)
        )
        .build()

      running(application) {
        val request = FakeRequest(POST, routes.MessagesController.post(departureWithoutMessages.departureId).url)
          .withHeaders("channel" -> departureWithoutMessages.channel.toString)
          .withXmlBody(declarationCancellationRequestXmlBody)

        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }

  "getMessages" - {

    "must return OK" - {
      "with the retrieved messages" in {

        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(1), status = SubmissionSucceeded)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.one(message), eoriNumber = "eori")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          lazy val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          status(result) mustEqual OK

          val expectedMessages  = ResponseMessage.build(departure.departureId, message)
          val expectedDeparture = ResponseDepartureWithMessages.build(departure, receivedSince = None).copy(messages = Seq(expectedMessages))

          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }

      "with only messages that are successful" in {
        val message1  = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(1), status = SubmissionSucceeded)
        val message2  = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(2), status = SubmissionFailed)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

        val expectedMessages  = ResponseMessage.build(departure.departureId, message1)
        val expectedDeparture = ResponseDepartureWithMessages.build(departure, receivedSince = None).copy(messages = Seq(expectedMessages))

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }

      }

      "with only messages that are successful and stateless" in {
        val message1 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(1), status = SubmissionSucceeded)
        val message2 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(2), status = SubmissionFailed)
        val message3 = Arbitrary.arbitrary[MessageWithoutStatus].sample.value.copy(messageId = MessageId(3))

        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2, message3), eoriNumber = "eori")

        val expectedMessage1  = ResponseMessage.build(departure.departureId, message1)
        val expectedMessage3  = ResponseMessage.build(departure.departureId, message3)
        val expectedDeparture = ResponseDepartureWithMessages.build(departure, receivedSince = None).copy(messages = Seq(expectedMessage1, expectedMessage3))

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }

      "with no messages if they are all failures" in {
        val message1 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)
        val message2 = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)

        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2), eoriNumber = "eori")

        val expectedDeparture = ResponseDepartureWithMessages.build(departure, receivedSince = None).copy(messages = Nil)

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }

      "with only messages received after the requested datetime" in {
        val requestedDateTime       = LocalDateTime.of(2021, 5, 11, 16, 42, 12)
        val requestedOffsetDateTime = requestedDateTime.atOffset(ZoneOffset.UTC)

        val message1 =
          Arbitrary
            .arbitrary[MessageWithStatus]
            .sample
            .value
            .copy(
              messageId = MessageId(1),
              status = SubmissionSucceeded,
              received = Some(LocalDateTime.of(2021, 5, 11, 15, 10, 32))
            )
        val message2 =
          Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(2), status = SubmissionSucceeded, received = Some(requestedDateTime))
        val message3 =
          Arbitrary
            .arbitrary[MessageWithStatus]
            .sample
            .value
            .copy(messageId = MessageId(3), status = SubmissionSucceeded, received = Some(LocalDateTime.of(2021, 5, 12, 17, 5, 24)))

        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message1, message2, message3), eoriNumber = "eori")

        val expectedMessage2 = ResponseMessage.build(departure.departureId, message2)
        val expectedMessage3 = ResponseMessage.build(departure.departureId, message3)

        val expectedDeparture =
          ResponseDepartureWithMessages.build(departure, receivedSince = Some(requestedOffsetDateTime)).copy(messages = Seq(expectedMessage2, expectedMessage3))

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId, receivedSince = Some(requestedOffsetDateTime)).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJson(expectedDeparture)
        }
      }
    }

    "must return NOT FOUND" - {
      "when departure is not found" in {
        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(DepartureId(1)).url)
            .withHeaders("channel" -> Web.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }

      "when departure is inaccessible to the user" in {
        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(messages = NonEmptyList.of(message), eoriNumber = "eori2")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.get(any(), any()))
          .thenReturn(Future.successful(Some(departure)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessages(departure.departureId).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }
    }

  }

  "getMessage" - {
    "must return Ok with the retrieved message and state SubmissionSuccessful" in {

      val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(messageId = MessageId(1), status = SubmissionSucceeded)
      val departure = Arbitrary.arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori")

      val mockDepartureRepository = mock[DepartureRepository]
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departure)))
      when(mockDepartureRepository.getMessage(any(), any(), any()))
        .thenReturn(Future.successful(Some(message)))

      val application =
        baseApplicationBuilder
          .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
          .build()

      running(application) {
        val request = FakeRequest(GET, routes.MessagesController.getMessage(departure.departureId, MessageId(1)).url)
          .withHeaders("channel" -> departure.channel.toString)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseMessage.build(departure.departureId, message))
      }
    }

    "must return Ok with the retrieved message when it has no state" in {
      val message   = Arbitrary.arbitrary[MessageWithoutStatus].sample.value.copy(messageId = MessageId(1))
      val departure = Arbitrary.arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori")

      val mockDepartureRepository = mock[DepartureRepository]
      when(mockDepartureRepository.getWithoutMessages(any(), any()))
        .thenReturn(Future.successful(Some(departure)))
      when(mockDepartureRepository.getMessage(any(), any(), any()))
        .thenReturn(Future.successful(Some(message)))

      val application =
        baseApplicationBuilder
          .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
          .build()

      running(application) {
        val request = FakeRequest(GET, routes.MessagesController.getMessage(departure.departureId, MessageId(1)).url)
          .withHeaders("channel" -> departure.channel.toString)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseMessage.build(departure.departureId, message))
      }
    }

    "must return NOT FOUND" - {
      "when departure is not found" in {
        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.getWithoutMessages(any(), any()))
          .thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(DepartureId(1), MessageId(1)).url)
            .withHeaders("channel" -> Web.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }

      "when message does not exist" in {
        val departure = Arbitrary.arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.getWithoutMessages(any(), any()))
          .thenReturn(Future.successful(Some(departure)))
        when(mockDepartureRepository.getMessage(any(), any(), any()))
          .thenReturn(Future.successful(None))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(departure.departureId, MessageId(6)).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }

      "when status is not equal to successful" in {
        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionFailed)
        val departure = Arbitrary.arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.getWithoutMessages(any(), any()))
          .thenReturn(Future.successful(Some(departure)))
        when(mockDepartureRepository.getMessage(any(), any(), any()))
          .thenReturn(Future.successful(Some(message)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(departure.departureId, MessageId(1)).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }

      "when message belongs to a departure the user cannot access" in {
        val message   = Arbitrary.arbitrary[MessageWithStatus].sample.value.copy(status = SubmissionSucceeded)
        val departure = Arbitrary.arbitrary[DepartureWithoutMessages].sample.value.copy(eoriNumber = "eori2")

        val mockDepartureRepository = mock[DepartureRepository]
        when(mockDepartureRepository.getWithoutMessages(any(), any()))
          .thenReturn(Future.successful(Some(departure)))
        when(mockDepartureRepository.getMessage(any(), any(), any()))
          .thenReturn(Future.successful(Some(message)))

        val application =
          baseApplicationBuilder
            .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
            .build()

        running(application) {
          val request = FakeRequest(GET, routes.MessagesController.getMessage(departure.departureId, MessageId(1)).url)
            .withHeaders("channel" -> departure.channel.toString)
          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
        }
      }

    }
  }

}
