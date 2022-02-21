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

package services

import base.SpecBase
import cats.data.NonEmptyList
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult._
import generators.ModelGenerators
import models.ChannelType.Web
import models.MessageStatus.SubmissionFailed
import models.MessageStatus.SubmissionPending
import models.Departure
import models.DepartureId
import models.DepartureIdSelector
import models.MessageId
import models.MessageStatus
import models.MessageStatusUpdate
import models.MessageType
import models.MessageWithStatus
import models.SubmissionProcessingResult
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionFailureRejected
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.inject.bind
import play.api.test.Helpers.running
import repositories.DepartureRepository
import uk.gov.hmrc.http.GatewayTimeoutException
import uk.gov.hmrc.http.UpstreamErrorResponse
import utils.Format
import utils.JsonHelper

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SubmitMessageServiceSpec extends SpecBase with JsonHelper with ScalaCheckDrivenPropertyChecks with ModelGenerators with IntegrationPatience {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val requestXmlBody =
    <CC015B>
      <SynVerNumMES2>123</SynVerNumMES2>
      <DatOfPreMES9>{Format.dateFormatted(localDate)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>abc</RefNumHEA4>
      </HEAHEA>
    </CC015B>

  val requestJsonBody = convertXmlToJson(requestXmlBody.toString())

  val messageId = MessageId(1)

  val movementMessage = MessageWithStatus(
    messageId,
    localDateTime,
    MessageType.DepartureDeclaration,
    requestXmlBody,
    SubmissionPending,
    2
  )

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
  } yield {
    departure.copy(
      eoriNumber = "eori",
      messages = NonEmptyList.one(movementMessage),
      nextMessageCorrelationId = movementMessage.messageCorrelationId
    )
  }

  "submit a new message" - {
    "return SubmissionSuccess and set the message status to submitted when the message is successfully saved, submitted" in {
      lazy val mockDepartureRepository: DepartureRepository = mock[DepartureRepository]
      lazy val mockMessageConnector: MessageConnector       = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value.copy(messageId = messageId)

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(
          eqTo(DepartureIdSelector(departureId)),
          eqTo(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded))
        )(any())

      }
    }

    "return SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Failure(new Throwable())))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value.copy(messageId = messageId)

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(
          eqTo(DepartureIdSelector(departureId)),
          eqTo(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded))
        )(any())
      }

    }

    "return SubmissionFailureInternal when the message is successfully saved and submitted, but the state of message is not updated due to exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
      }
    }

    "return SubmissionFailureInternal when the message is not saved" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Failure(new Exception)))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(departureId), eqTo(movementMessage), any(), any())(any())
      }

    }

    "return SubmissionFailureExternal and set the message status to SubmissionFailed when the message successfully saves, but the external service fails on submission" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        forAll(arbitrary[DepartureId], arbitrary[MessageId], arbitrary[MessageWithStatus], arbitrary[EisSubmissionFailureDownstream]) {
          (departureId, messageId, movementMessage, submissionFailure) =>
            when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
            when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(submissionFailure))
            when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))
            when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

            val expectedMessage  = movementMessage.copy(messageId = messageId)
            val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

            val result = service.submitMessage(departureId, expectedMessage, Web)

            result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal

            verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(expectedMessage))
            verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(expectedMessage), any(), any())(any())
            verify(mockDepartureRepository, times(1)).updateDeparture(any(), eqTo(expectedModifier))(any())

        }
      }
    }

    "return SubmissionFailureRejected and set the message status to SubmissionFailed when the message successfully saves, but the external service rejects the message" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value.copy(messageId = messageId)

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)

        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(movementMessage))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(any(), eqTo(expectedModifier))(any())
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS and the departure has failed to update with an exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureExternal if there has been a rejection from EIS with an unknown status code" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(UnexpectedHttpResponse(UpstreamErrorResponse("", 501))))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }

    "return SubmissionFailureExternal if there has been a downstream failure with EIS and the departure has failed to update with an exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(UnexpectedHttpResponse(UpstreamErrorResponse("", 501))))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }

    "return SubmissionFailureExternal if there has been a timeout exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.failed(new GatewayTimeoutException("")))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val departureId     = arbitrary[DepartureId].sample.value
        val movementMessage = arbitrary[MessageWithStatus].sample.value

        val result = service.submitMessage(departureId, movementMessage, Web)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }
  }

  "submit a new departure" - {
    val departureWithOneMovementGenerator = arbitrary[Departure].map(_.copy(messages = NonEmptyList.one(movementMessage)))
    val departure                         = departureWithOneMovementGenerator.sample.value

    "return SubmissionSuccess and set the message status to submitted when the message is successfully saved, submitted" in {
      lazy val mockDepartureRepository: DepartureRepository = mock[DepartureRepository]
      lazy val mockMessageConnector: MessageConnector       = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(
          eqTo(DepartureIdSelector(departure.departureId)),
          eqTo(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded))
        )(any())

      }
    }

    "return SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Failure(new Throwable())))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(
          eqTo(DepartureIdSelector(departure.departureId)),
          eqTo(MessageStatusUpdate(messageId, MessageStatus.SubmissionSucceeded))
        )(any())
      }

    }

    "return SubmissionFailureInternal when the message is successfully saved and submitted, but the state of message is not updated due to an exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(EisSubmissionSuccessful))
      when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
      }

    }

    "return SubmissionFailureInternal when the message is not saved" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.failed(new Exception))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(departure.departureId), eqTo(movementMessage), any(), any())(any())
      }

    }

    "return SubmissionFailureExternal and set the message status to SubmissionFailed when the message successfully saves, but the external service fails on submission" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        forAll(departureWithOneMovementGenerator, arbitrary[EisSubmissionFailureDownstream]) {
          (departure, submissionResult) =>
            Mockito.reset(mockDepartureRepository, mockMessageConnector)

            when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
            when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(submissionResult))
            when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

            val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

            val result = service.submitDeparture(departure)

            result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal
            verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
            verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(movementMessage), any(), any())(any())
            verify(mockDepartureRepository, times(1)).updateDeparture(any(), eqTo(expectedModifier))(any())
        }
      }
    }

    "return SubmissionFailureRejected and set the message status to SubmissionFailed when the message successfully saves, but the external service rejects the message" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val expectedModifier = MessageStatusUpdate(messageId, SubmissionFailed)

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)

        verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(movementMessage), any(), any())(any())
        verify(mockDepartureRepository, times(1)).updateDeparture(any(), eqTo(expectedModifier))(any())
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS due to virus found or invalid token" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureInternal if there has been a rejection from EIS and the departure has failed to update with an exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(VirusFoundOrInvalidToken))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureInternal
      }
    }

    "return SubmissionFailureRejected if there has been a rejection from EIS due to error in payload" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(ErrorInPayload))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureRejected(ErrorInPayload.responseBody)
      }
    }

    "return SubmissionFailureExternal if there has been a rejection from EIS with an unknown status code" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(UnexpectedHttpResponse(UpstreamErrorResponse("", 501))))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }

    "return SubmissionFailureExternal if there has been a downstream failure with EIS and the departure has failed to update with an exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.successful(UnexpectedHttpResponse(UpstreamErrorResponse("", 501))))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.failed(new Exception("failed")))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }

    "return SubmissionFailureExternal if there has been a timeout exception" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector    = mock[MessageConnector]

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {
        val service = application.injector.instanceOf[SubmitMessageService]

        when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
        when(mockMessageConnector.post(any(), any(), any(), any())(any())).thenReturn(Future.failed(new GatewayTimeoutException("")))
        when(mockDepartureRepository.updateDeparture(any(), any())(any())).thenReturn(Future.successful(Success(())))

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionFailureExternal
      }
    }

  }
}
