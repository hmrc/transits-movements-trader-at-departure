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

package services

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import cats.data.NonEmptyList
import connectors.MessageConnector
import generators.ModelGenerators
import models.MessageStatus.SubmissionPending
import models.{Departure, DepartureId, DepartureStatus, MessageId, MessageStatus, MessageType, MessageWithStatus, SubmissionProcessingResult}
import org.mockito.Matchers.{eq => eqTo, _}
import org.mockito.Mockito.when
import org.mockito.Mockito._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import play.api.inject.bind
import play.api.test.Helpers.ACCEPTED
import play.api.test.Helpers.running
import repositories.DepartureRepository
import uk.gov.hmrc.http.HttpResponse
import utils.Format

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Failure
import scala.util.Success

class SubmitMessageServiceSpec extends SpecBase with ModelGenerators {

  val localDate     = LocalDate.now()
  val localTime     = LocalTime.of(1, 1)
  val localDateTime = LocalDateTime.of(localDate, localTime)

  val ref = "ref"

  val requestXmlBody =
    <CC015B>
      <DatOfPreMES9>{Format.dateFormatted(localDateTime)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(localDateTime)}</TimOfPreMES10>
      <HEAHEA>
        <RefNumHEA4>{ref}</RefNumHEA4>
      </HEAHEA>
    </CC015B>

  val messageId = MessageId.fromIndex(0)

  val message = MessageWithStatus(
    localDateTime,
    MessageType.DepartureDeclaration,
    requestXmlBody,
    SubmissionPending,
    2
  )

  val departureWithOneMessage: Gen[Departure] = for {
    arrival <- arbitrary[Departure]
  } yield {
    arrival.copy(
      eoriNumber = "eori",
      status = DepartureStatus.DepartureSubmitted,
      messages = NonEmptyList.one(message),
      nextMessageCorrelationId = message.messageCorrelationId
    )
  }

  "submit a new message" - {
    "return SubmissionProcessingResult.SubmissionSuccess when the message is successfully saved, submitted and the state is updated" in {
      lazy val mockDepartureRepository: DepartureRepository = mock[DepartureRepository]
      lazy val mockMessageConnector: MessageConnector       = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockDepartureRepository.setDepartureStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(Some(())))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId       = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val message = arbitrary[MessageWithStatus].sample.value
        val departureStatus   = DepartureStatus.DepartureSubmitted

        val result = service.submitMessage(departureId, messageId.index, message, departureStatus)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess

        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(message))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setDepartureStateAndMessageState(eqTo(departureId),
          eqTo(messageId.index),
          eqTo(DepartureStatus.DepartureSubmitted),
          eqTo(MessageStatus.SubmissionSucceeded))

      }

    }

    "return SubmissionProcessingResult.SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockDepartureRepository.setDepartureStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(None))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId       = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val message = arbitrary[MessageWithStatus].sample.value
        val departureStatus   = DepartureStatus.DepartureSubmitted

        val result = service.submitMessage(departureId, messageId.index, message, departureStatus)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(message))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setDepartureStateAndMessageState(eqTo(departureId),
          eqTo(messageId.index),
          eqTo(DepartureStatus.DepartureSubmitted),
          eqTo(MessageStatus.SubmissionSucceeded))
      }

    }

    "return SubmissionProcessingResult.SubmissionFailureInternal when the message is not saved" in {
      val mockDepartureRepository = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Failure(new Exception)))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val departureId     = arbitrary[DepartureId].sample.value
        val messageId       = arbitrary[MessageId].sample.value
        val message = arbitrary[MessageWithStatus].sample.value
        val departureStatus   = arbitrary[DepartureStatus].sample.value

        val result = service.submitMessage(departureId, messageId.index, message, departureStatus)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureInternal
        verify(mockMessageConnector, never()).post(eqTo(departureId), eqTo(message), any())(any())
      }

    }

    "return SubmissionProcessingResult.SubmissionFailureExternal when the message successfully saves, but is not submitted and set the message state to SubmissionFailed" in {
      val mockDepartureRepository       = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockDepartureRepository.addNewMessage(any(), any())).thenReturn(Future.successful(Success(())))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.failed(new Exception))
      when(mockDepartureRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))

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
        val message         = arbitrary[MessageWithStatus].sample.value
        val departureStatus = arbitrary[DepartureStatus].sample.value

        val result = service.submitMessage(departureId, messageId.index, message, departureStatus)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal
        verify(mockDepartureRepository, times(1)).addNewMessage(eqTo(departureId), eqTo(message))
        verify(mockMessageConnector, times(1)).post(eqTo(departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setMessageState(eqTo(departureId), eqTo(messageId.index), eqTo(MessageStatus.SubmissionFailed))
      }

    }

  }

  "submit a new departure" - {
    val departure = arbitrary[Departure].sample.value.copy(messages = NonEmptyList.one(message))

    "return SubmissionProcessingResult.SubmissionSuccess when the message is successfully saved, submitted and the state is updated" in {
      lazy val mockDepartureRepository: DepartureRepository = mock[DepartureRepository]
      lazy val mockMessageConnector: MessageConnector       = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
      when(mockDepartureRepository.setDepartureStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(Some(())))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = Await.result(service.submitDeparture(departure), Duration.Inf)

        result mustEqual SubmissionProcessingResult.SubmissionSuccess
        verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setDepartureStateAndMessageState(
          eqTo(departure.departureId),
          eqTo(messageId.index),
          eqTo(DepartureStatus.DepartureSubmitted),
          eqTo(MessageStatus.SubmissionSucceeded)
        )
      }
    }

    "return SubmissionProcessingResult.SubmissionSuccess when the message is successfully saved and submitted, but the state of message is not updated" in {
      val mockDepartureRepository       = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.successful(HttpResponse(ACCEPTED)))
      when(mockDepartureRepository.setDepartureStateAndMessageState(any(), any(), any(), any())).thenReturn(Future.successful(None))

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
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setDepartureStateAndMessageState(eqTo(departure.departureId),
          eqTo(messageId.index),
          eqTo(DepartureStatus.DepartureSubmitted),
          eqTo(MessageStatus.SubmissionSucceeded))
      }

    }

    "return SubmissionProcessingResult.SubmissionFailureInternal when the message is not saved" in {
      val mockDepartureRepository       = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

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
        verify(mockMessageConnector, never()).post(eqTo(departure.departureId), eqTo(message), any())(any())
      }

    }

    "return SubmissionProcessingResult.SubmissionFailureExternal when the message successfully saves, but is not submitted and set the message state to SubmissionFailed" in {
      val mockDepartureRepository       = mock[DepartureRepository]
      val mockMessageConnector          = mock[MessageConnector]

      when(mockDepartureRepository.insert(any())).thenReturn(Future.successful(()))
      when(mockMessageConnector.post(any(), any(), any())(any())).thenReturn(Future.failed(new Exception))
      when(mockDepartureRepository.setMessageState(any(), any(), any())).thenReturn(Future.successful((Success(()))))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[MessageConnector].toInstance(mockMessageConnector)
        )
        .build()

      running(application) {

        val service = application.injector.instanceOf[SubmitMessageService]

        val result = service.submitDeparture(departure)

        result.futureValue mustEqual SubmissionProcessingResult.SubmissionFailureExternal
        verify(mockDepartureRepository, times(1)).insert(eqTo(departure))
        verify(mockMessageConnector, times(1)).post(eqTo(departure.departureId), eqTo(message), any())(any())
        verify(mockDepartureRepository, times(1)).setMessageState(eqTo(departure.departureId), eqTo(messageId.index), eqTo(MessageStatus.SubmissionFailed))
      }

    }

  }

}
