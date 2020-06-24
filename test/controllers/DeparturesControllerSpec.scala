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

import java.time.{LocalDate, LocalDateTime, LocalTime}

import base.SpecBase
import controllers.actions.{AuthenticateGetOptionalDepartureForWriteActionProvider, FakeAuthenticatedGetOptionalDepartureForWriteActionProvider}
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.BeforeAndAfterEach
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import generators.ModelGenerators
import org.mockito.Matchers.any
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.{POST, header, route, running, status}
import play.api.test.Helpers._
import cats.data.NonEmptyList
import models.MessageStatus.{SubmissionFailed, SubmissionPending, SubmissionSucceeded}
import models.SubmissionProcessingResult.{SubmissionFailureExternal, SubmissionFailureInternal, SubmissionSuccess}
import models.{Departure, DepartureId, DepartureStatus, MessageType, MessageWithStatus, SubmissionProcessingResult}
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.{DepartureIdRepository, DepartureRepository}
import services.SubmitMessageService
import utils.Format

import scala.concurrent.Future
import scala.util.Success

class DeparturesControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val newDepartureId = arbitrary[DepartureId].sample.value

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
    referenceNumber = "referenceNumber",
    status = DepartureStatus.Initialized,
    created = localDateTime,
    updated = localDateTime,
    nextMessageCorrelationId = message.messageCorrelationId + 1,
    messages = NonEmptyList.one(message)
  )

  val requestXmlBody =
    <CC015B>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>abc</RefNumHEA4>
      </HEAHEA>
    </CC015B>

    "/POST" - {
      "when there are no previous failed attempts to submit" - {

        "must return Accepted, create departure, send the message upstream and return the correct location header if submission successful" in {
          val mockDepartureIdRepository = mock[DepartureIdRepository]
          val mockDepartureRepository = mock[DepartureRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(initializedDeparture.departureId))
          when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(Success(())))
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
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider()))
            .build()

          running(application) {

            val request = FakeRequest(POST, routes.DeparturesController.post().url).withXmlBody(requestXmlBody)

            val result = route(application, request).value

            status(result) mustEqual INTERNAL_SERVER_ERROR
          }
        }

        "must return InternalServerError if there was an internal failure when saving and sending" in {
          val mockDepartureIdRepository = mock[DepartureIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

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
          val mockSubmitMessageService = mock[SubmitMessageService]

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
          val mockDepartureIdRepository  = mock[DepartureIdRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

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
            verify(mockDepartureIdRepository, never()).nextId()
          }
        }

        "must return BadRequest if the message is not an departure declaration" in {
          val mockDepartureIdRepository = mock[DepartureIdRepository]

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
            verify(mockDepartureIdRepository, never()).nextId()
          }
        }
      }

      "when there has been a previous failed attempt to submit" - {

        val failedToSubmit007     = message.copy(status = SubmissionFailed)
        val failedToSubmitDeparture = initializedDeparture.copy(messages = NonEmptyList.one(failedToSubmit007))

        "must return Accepted when submitted to upstream against the existing departure" in {
          val mockDepartureRepository = mock[DepartureRepository]
          val mockSubmitMessageService = mock[SubmitMessageService]

          when(mockSubmitMessageService.submitMessage(any(),any(),any(),any())(any())).thenReturn(Future.successful(SubmissionSuccess))

          val application = baseApplicationBuilder
            .overrides(
              bind[SubmitMessageService].toInstance(mockSubmitMessageService),
              bind[DepartureRepository].toInstance(mockDepartureRepository),
              bind[AuthenticateGetOptionalDepartureForWriteActionProvider].toInstance(FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(failedToSubmitDeparture))
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

        "must return Accepted when saved as a new arrival movement when there has been a successful message in an older one" in {
          val messages = NonEmptyList.of(
            message.copy(status = SubmissionPending, messageCorrelationId = 1),
            message.copy(status = SubmissionFailed, messageCorrelationId = 2),
            message.copy(status = SubmissionSucceeded, messageCorrelationId = 3)
          )
          val departure = initializedDeparture.copy(messages = messages, nextMessageCorrelationId = 4)

          val expectedDeparture = initializedDeparture.copy(messages = NonEmptyList.of(message))

          val mockSubmitMessageService = mock[SubmitMessageService]
          val mockDepartureIdRepository  = mock[DepartureIdRepository]

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

          when(mockSubmitMessageService.submitMessage(any(),any(),any(),any())(any()))
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

          when(mockSubmitMessageService.submitMessage(any(),any(),any(),any())(any()))
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

        "must return BadRequest if the message is not an arrival notification" in {
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
      
    }
}