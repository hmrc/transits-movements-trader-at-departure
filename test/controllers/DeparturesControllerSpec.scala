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
import controllers.actions.AuthenticateGetOptionalDepartureForWriteActionProvider
import controllers.actions.FakeAuthenticatedGetOptionalDepartureForWriteActionProvider
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.BeforeAndAfterEach
import org.scalatest.stats
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import generators.ModelGenerators
import org.mockito.ArgumentMatchers.any
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.POST
import play.api.test.Helpers.header
import play.api.test.Helpers.route
import play.api.test.Helpers.running
import play.api.test.Helpers.status
import play.api.test.Helpers._
import cats.data.NonEmptyList
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageType
import models.MessageWithStatus
import models.SubmissionProcessingResult
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
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
    eoriNumber = "eori",
    movementReferenceNumber = None,
    referenceNumber = "referenceNumber",
    status = DepartureStatus.Initialized,
    created = localDateTime,
    updated = localDateTime,
    nextMessageCorrelationId = message.messageCorrelationId + 1,
    messages = NonEmptyList.one(message)
  )

  "/POST" - {
    "when there are no previous failed attempts to submit" - {

      "must return Accepted, create departure, send the message upstream and return the correct location header if submission successful" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]
        val mockDepartureRepository   = mock[DepartureRepository]
        val mockSubmitMessageService  = mock[SubmitMessageService]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(initializedDeparture.departureId))
        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockSubmitMessageService.submitDeparture(any())(any())).thenReturn(Future.successful((SubmissionSuccess)))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          verify(mockSubmitMessageService, times(1)).submitDeparture(any())(any())
          header("Location", result).get must be(routes.DeparturesController.get(initializedDeparture.departureId).url)
        }
      }

      "must return InternalServerError if the DepartureId generation fails" in {
        val mockDepartureIdRepository = mock[DepartureIdRepository]

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.failed(new Exception))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

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
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

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
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

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
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
            )
            .build()

        running(application) {
          val requestXmlBody =
            <CC015B></CC015B>

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

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
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider())
            )
            .build()

        running(application) {
          val requestXmlBody = <InvalidRootNode></InvalidRootNode>

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
          header("Location", result) must not be (defined)
        }
      }
    }

    "when there has been a previous failed attempt to submit" - {

      val failedToSubmit007       = message.copy(status = SubmissionFailed)
      val failedToSubmitDeparture = initializedDeparture.copy(messages = NonEmptyList.one(failedToSubmit007))

      "must return Accepted when submitted to upstream against the existing departure" in {
        val mockDepartureRepository  = mock[DepartureRepository]
        val mockSubmitMessageService = mock[SubmitMessageService]

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any())).thenReturn(Future.successful(SubmissionSuccess))

        val application = baseApplicationBuilder
          .overrides(
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(
              FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(failedToSubmitDeparture))
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          verify(mockSubmitMessageService, times(1)).submitMessage(any(), any(), any(), any())(any())
          header("Location", result).get must be(routes.DeparturesController.get(failedToSubmitDeparture.departureId).url)
        }
      }

      "must return Accepted when saved as a new departure declaration when there has been a successful message in an older one" in {
        val messages = NonEmptyList.of(
          message.copy(status = SubmissionPending, messageCorrelationId = 1),
          message.copy(status = SubmissionFailed, messageCorrelationId = 2),
          message.copy(status = SubmissionSucceeded, messageCorrelationId = 3)
        )
        val departure = initializedDeparture.copy(messages = messages, nextMessageCorrelationId = 4)

        val expectedDeparture = initializedDeparture.copy(messages = NonEmptyList.of(message))

        val mockSubmitMessageService  = mock[SubmitMessageService]
        val mockDepartureIdRepository = mock[DepartureIdRepository]

        when(mockSubmitMessageService.submitDeparture(any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

        when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(expectedDeparture.departureId))

        val application = baseApplicationBuilder
          .overrides(
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(departure))
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual ACCEPTED
          header("Location", result).value must be(routes.DeparturesController.get(expectedDeparture.departureId).url)
          verify(mockSubmitMessageService, times(1)).submitDeparture(any())(any())
        }
      }

      "must return InternalServerError if there was an internal failure when saving and sending" in {
        val mockSubmitMessageService = mock[SubmitMessageService]

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

        val application = baseApplicationBuilder
          .overrides(
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(
              FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(initializedDeparture))
          )
          .build()

        running(application) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR

        }
      }

      "must return BadGateway if there was an external failure when saving and sending" in {
        val mockSubmitMessageService = mock[SubmitMessageService]

        when(mockSubmitMessageService.submitMessage(any(), any(), any(), any())(any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

        val app = baseApplicationBuilder
          .overrides(
            bind[SubmitMessageService].toInstance(mockSubmitMessageService),
            bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(
              FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(initializedDeparture))
          )
          .build()

        running(app) {

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(app, request).value

          status(result) mustEqual BAD_GATEWAY
        }
      }

      "must return BadRequest if the payload is malformed" in {
        val departure = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori")

        val application =
          baseApplicationBuilder
            .overrides(
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(departure))
            )
            .build()

        running(application) {
          val requestXmlBody = <CC015B><HEAHEA></HEAHEA></CC015B>

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest if the message is not a departure declaration" in {
        val application =
          baseApplicationBuilder
            .overrides(
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(
                FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(initializedDeparture))
            )
            .build()

        running(application) {

          val requestXmlBody = <InvalidRootNode></InvalidRootNode>

          val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
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
      when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)

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

      when(mockDepartureRepository.get(any())).thenReturn(Future.successful(None))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
        val result  = route(application, request).value

        status(result) mustEqual NOT_FOUND
      }
    }

    "must return Not Found if departure eori doesn't match" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      val departure = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori2")
      when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
        val result  = route(application, request).value

        status(result) mustEqual NOT_FOUND
      }
    }

    "must return an INTERNAL_SERVER_ERROR when we cannot retrieve the departure" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.get(any()))
        .thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.get(DepartureId(1)).url)
        val result  = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }
  "/GET" - {
    "must return all departures from database" in {
      val mockDepartureRepository = mock[DepartureRepository]

      val departure         = Arbitrary.arbitrary[Departure].sample.value.copy(eoriNumber = "eori")
      val responseDeparture = ResponseDeparture.build(departure)

      when(mockDepartureRepository.fetchAllDepartures(any())).thenReturn(Future.successful(Seq(departure)))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseDepartures(Seq(responseDeparture)))
      }
    }

    "must return empty sequence when there are no departures in database" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.fetchAllDepartures(any())).thenReturn(Future.successful(Seq.empty))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.toJson(ResponseDepartures(Seq.empty))
      }
    }
    "must return INTERNAL_SERVER_ERROR when we cannot retrieve departures" in {
      val mockDepartureRepository = mock[DepartureRepository]

      when(mockDepartureRepository.fetchAllDepartures(any())).thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository))
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.DeparturesController.getDepartures().url)
        val result  = route(application, request).value

        status(result) mustEqual INTERNAL_SERVER_ERROR
      }
    }
  }
}
