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
import audit.AuditService
import audit.AuditType.DepartureDeclarationSubmitted
import audit.AuditType.MesSenMES3Added
import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ChannelType.api
import models.ChannelType.web
import models.MessageStatus.SubmissionPending
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import models.response.ResponseDeparture
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import models.response.ResponseDepartures
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageType
import models.MessageWithStatus
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.POST
import play.api.test.Helpers.header
import play.api.test.Helpers.route
import play.api.test.Helpers.running
import play.api.test.Helpers.status
import play.api.test.Helpers._
import repositories.DepartureIdRepository
import repositories.DepartureRepository
import services.SubmitMessageService
import utils.Format

import scala.concurrent.Future

class DeparturesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val newDepartureId = arbitrary[DepartureId].sample.value

  val requestXmlBody =
    <CC015B>
      <SynVerNumMES2>123</SynVerNumMES2>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>abc</RefNumHEA4>
      </HEAHEA>
    </CC015B>

  val message = MessageWithStatus(
    localDateTime,
    MessageType.DepartureDeclaration,
    requestXmlBody,
    SubmissionPending,
    1
  )

  val initializedDeparture = Departure(
    departureId = newDepartureId,
    channel = api,
    eoriNumber = "eori",
    movementReferenceNumber = None,
    status = DepartureStatus.Initialized,
    created = localDateTime,
    updated = localDateTime,
    nextMessageCorrelationId = message.messageCorrelationId + 1,
    messages = NonEmptyList.one(message),
    referenceNumber = "referenceNumber"
  )

  "/POST" - {
    "when there are no previous failed attempts to submit" - {

      "must return Accepted, create departure, send the message upstream and return the correct location header if submission successful" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockDepartureRepository   = mock[DepartureRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]
        val mockAuditService          = mock[AuditService]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(initializedDeparture.departureId))
        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful((SubmissionSuccess)))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url)
            .withHeaders("channel" -> initializedDeparture.channel.toString)
            .withXmlBody(requestXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual ACCEPTED
          verify(mockSubmitMessageService, times(1)).submitDeparture(any())(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(DepartureDeclarationSubmitted), any(), any())(any())
          verify(mockAuditService, times(1)).auditEvent(eqTo(MesSenMES3Added), any(), any())(any())
          header("Location", result).get must be(routes.DeparturesController.get(initializedDeparture.departureId).url)
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
