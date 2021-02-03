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

package controllers

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import audit.AuditService
import audit.AuditType
import base.SpecBase
import cats.data.NonEmptyList
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult.ErrorInPayload
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import controllers.actions.FakeAuthenticateActionProvider
import controllers.actions.FakeAuthenticatedGetDepartureForReadActionProvider
import generators.ModelGenerators
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.ChannelType.api
import models.ChannelType.web
import models.MessageId
import models.MessageSender
import models.MessageType
import models.MessageWithStatus
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionFailureRejected
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.DepartureIdRepository
import repositories.DepartureRepository
import repositories.LockRepository
import services.SubmitMessageService
import utils.Format

import scala.concurrent.Future
import scala.xml.Utility.trim
import scala.xml.NodeSeq

class DeparturesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val departureId = arbitrary[DepartureId].sample.value
  val mrn         = arbitrary[MovementReferenceNumber].sample.value

  val requestXmlBody =
    <CC015B>
      <SynVerNumMES2>123</SynVerNumMES2>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>{mrn.value}</RefNumHEA4>
      </HEAHEA>
    </CC015B>

  def savedXmlMessage(messageCorrelationId: Int) =
    <CC015B>
      <SynVerNumMES2>123</SynVerNumMES2>
      <MesSenMES3>{MessageSender(departureId, messageCorrelationId).toString}</MesSenMES3>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>{mrn.value}</RefNumHEA4>
      </HEAHEA>
    </CC015B>

  def movementMessage(messageCorrelationId: Int): MessageWithStatus = MessageWithStatus(
    localDateTime,
    MessageType.DepartureDeclaration,
    savedXmlMessage(messageCorrelationId).map(trim),
    SubmissionPending,
    1
  )

  val initializedDeparture = Departure(
    departureId = departureId,
    channel = api,
    eoriNumber = "eori",
    movementReferenceNumber = None,
    status = DepartureStatus.Initialized,
    created = localDateTime,
    updated = localDateTime,
    nextMessageCorrelationId = movementMessage(1).messageCorrelationId + 1,
    messages = NonEmptyList.one(movementMessage(1)),
    referenceNumber = "referenceNumber"
  )

  "/POST" - {
    "when there are no previous failed attempts to submit" - {

      "must return Accepted, create departure, send the message upstream and return the correct location header if submission successful" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]
        val mockAuditService          = mock[AuditService]

        val expectedMessage: MessageWithStatus = movementMessage(1).copy(messageCorrelationId = 1)
        val newDeparture                       = initializedDeparture.copy(messages = NonEmptyList.of[MessageWithStatus](expectedMessage), channel = web)
        val captor: ArgumentCaptor[Departure]  = ArgumentCaptor.forClass(classOf[Departure])

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(newDeparture.departureId))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {

          val request =
            FakeRequest(POST, routes.DeparturesController.post().url)
              .withHeaders("channel" -> newDeparture.channel.toString)
              .withXmlBody(requestXmlBody.map(trim))

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.DeparturesController.get(newDeparture.departureId).url)

          verify(mockSubmitMessageService, times(1)).submitDeparture(captor.capture())(any())

          val departureMessage: MessageWithStatus = captor.getValue.messages.head.asInstanceOf[MessageWithStatus]
          departureMessage.message.map(trim) mustEqual expectedMessage.message.map(trim)

          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.DepartureDeclarationSubmitted), any(), any())(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(AuditType.MesSenMES3Added), any(), any())(any())
        }
      }

      "must return InternalServerError if the DepartureId generation fails" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        val departureId = DepartureId(1)

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(departureId))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful(SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual INTERNAL_SERVER_ERROR
          header("Location", result) must not be defined
        }
      }

      "must return BadGateway if there was an external failure when saving and sending" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        val departureId = DepartureId(1)

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(departureId))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful(SubmissionFailureExternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual BAD_GATEWAY
          header("Location", result) must not be defined
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(DepartureId(1)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            )
            .build()

        running(application) {
          val requestXmlBody =
            <CC015B></CC015B>

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustEqual "The value of element 'DatOfPreMES9' is not valid with respect to pattern 'yyyyMMdd'"
          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
        }
      }

      "must return BadRequest if the message has been rejected from EIS due to error in payload" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        val departureId = DepartureId(1)

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(departureId))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful(SubmissionFailureRejected(ErrorInPayload.responseBody)))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe "Message failed schema validation"
          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not an departure declaration" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(DepartureId(1)))

        val application =
          baseApplicationBuilder
            .overrides(
              bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            )
            .build()

        running(application) {
          val requestXmlBody = <InvalidRootNode></InvalidRootNode>

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustEqual "The root element name does not match 'CC015B'"
          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
        }
      }

      "must return InternalServerError if there has been a rejection from EIS due to virus found or invalid token" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        val departureId = DepartureId(1)

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(departureId))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> web.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }
    }
  }

  "/:departureId GET" - {

    "must return Ok with the retrieved departure" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      val departure = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori")
      when(mockDepartureRepository.get(any(), any())).thenReturn(Future.successful(Some(departure)))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
          .withHeaders("channel" -> departure.channel.toString)

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseDeparture.build(departure))
      }
    }

    "must return Not Found if departure doesn't exist" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      when(mockDepartureRepository.get(any(), any())).thenReturn(Future.successful(None))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
          .withHeaders("channel" -> web.toString)
        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual NOT_FOUND
      }
    }

    "must return Not Found if departure eori doesn't match" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      val departure = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori2")
      when(mockDepartureRepository.get(any(), any())).thenReturn(Future.successful(Some(departure)))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
          .withHeaders("channel" -> departure.channel.toString)
        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual NOT_FOUND
      }
    }

    "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the departure" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.get(any(), any()))
        .thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
          .withHeaders("channel" -> web.toString)
        val result = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }
  "/GET" - {
    "must return all departures from database" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val departure         = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori")
      val responseDeparture = ResponseDeparture.build(departure)

      when(mockDepartureRepository.fetchAllDepartures(any(), any())).thenReturn(Future.successful(Seq(departure)))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
          .withHeaders("channel" -> departure.channel.toString)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseDepartures(Seq(responseDeparture)))
      }
    }

    "must return empty sequence when there are no departures in database" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.fetchAllDepartures(any(), any())).thenReturn(Future.successful(Seq.empty))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
          .withHeaders("channel" -> web.toString)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseDepartures(Seq.empty))
      }
    }
    "must return INTERNAL_SERVER_ERROR when we cannot retrieve departures" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.fetchAllDepartures(any(), any())).thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
          .withHeaders("channel" -> web.toString)
        val result = route(application, request).value

        contentAsString(result) mustBe empty
        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }
}
