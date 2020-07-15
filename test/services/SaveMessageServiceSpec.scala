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
import java.time.LocalTime

import base.SpecBase
import models.DepartureId
import models.DepartureRejectedResponse
import models.DepartureStatus
import models.MrnAllocatedResponse
import models.MessageSender
import models.MovementReferenceNumber
import models.SubmissionProcessingResult
import models.XSDFile.MRNAllocatedXSD
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import play.api.inject.bind
import repositories.DepartureRepository
import utils.Format

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

class SaveMessageServiceSpec extends SpecBase with BeforeAndAfterEach {

  private val mockDepartureRepository  = mock[DepartureRepository]
  private val mockXmlValidationService = mock[XmlValidationService]

  override def beforeEach = {
    super.beforeEach()
    reset(mockDepartureRepository)
    reset(mockXmlValidationService)
  }

  "validateXmlAndSaveMessage" - {

    "Returns Success when we successfully save a message" in {
      when(mockDepartureRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Success(())))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val departureRejected =
        <CC016A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC016A>

      val result =
        saveMessageService.validateXmlAndSaveMessage(departureRejected, messageSender, DepartureRejectedResponse, DepartureStatus.DepartureRejected).futureValue

      result mustBe SubmissionProcessingResult.SubmissionSuccess
      verify(mockDepartureRepository, times(1)).addResponseMessage(eqTo(departureId), any(), eqTo(DepartureStatus.DepartureRejected))
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

    "return Failure when we cannot save the message" in {
      when(mockDepartureRepository.addResponseMessage(any(), any(), any())).thenReturn(Future.successful(Failure(new Exception)))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val departureRejected =
        <CC016A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC016A>

      val result =
        saveMessageService.validateXmlAndSaveMessage(departureRejected, messageSender, DepartureRejectedResponse, DepartureStatus.DepartureRejected).futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureInternal
      verify(mockDepartureRepository, times(1)).addResponseMessage(any(), any(), any())
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

    "return Failure when we cannot parse the message" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Failure(new Exception))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val requestInvalidXmlBody = <Invalid> invalid </Invalid>

      val result = saveMessageService
        .validateXmlAndSaveMessage(requestInvalidXmlBody, messageSender, DepartureRejectedResponse, DepartureStatus.DepartureRejected)
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureExternal
      verify(mockDepartureRepository, never()).addResponseMessage(any(), any(), any())
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

    "return Failure when we cannot parse the message due malformed time" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)
      val dateOfPrep           = LocalDate.now()
      val timeOfPrep           = LocalTime.of(1, 1)

      val requestInvalidXmlBody =
        <CC016A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) + "/"}</TimOfPreMES10>
        </CC016A>

      val result = saveMessageService
        .validateXmlAndSaveMessage(requestInvalidXmlBody, messageSender, DepartureRejectedResponse, DepartureStatus.DepartureRejected)
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureExternal
      verify(mockDepartureRepository, never()).addResponseMessage(any(), any(), any())
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }
  }

  "validateXmlSaveMessageUpdateMrn" - {
    "Returns Success when we successfully save a message" in {
      when(mockDepartureRepository.setMrnAndAddResponseMessage(any(), any(), any(), any())).thenReturn(Future.successful(Success(())))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val mrn                  = "mrn"
      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val mrnAllocated =
        <CC028A>
          <SynIdeMES1>abcd</SynIdeMES1>
          <SynVerNumMES2>12345671</SynVerNumMES2>
          <MesSenMES3>abc</MesSenMES3>
          <MesRecMES6>abc</MesRecMES6>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <IntConRefMES11>abc</IntConRefMES11>
          <MesIdeMES19>abc</MesIdeMES19>
          <MesTypMES20>abc</MesTypMES20>
          <HEAHEA>
            <RefNumHEA4>ref</RefNumHEA4>
            <DocNumHEA5>{mrn}</DocNumHEA5>
            <AccDatHEA158>123</AccDatHEA158>
          </HEAHEA>
          <TRAPRIPC1></TRAPRIPC1>
          <CUSOFFDEPEPT></CUSOFFDEPEPT>
        </CC028A>

      val result = saveMessageService
        .validateXmlSaveMessageUpdateMrn(mrnAllocated, messageSender, MrnAllocatedResponse, DepartureStatus.MrnAllocated, MovementReferenceNumber(mrn))
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionSuccess

      verify(mockDepartureRepository, times(1)).setMrnAndAddResponseMessage(eqTo(departureId),
                                                                            any(),
                                                                            eqTo(DepartureStatus.MrnAllocated),
                                                                            eqTo(MovementReferenceNumber(mrn)))
      verify(mockXmlValidationService, times(1)).validate(any(), any())

    }

    "return Failure when we cannot save the message" in {
      when(mockDepartureRepository.setMrnAndAddResponseMessage(any(), any(), any(), any())).thenReturn(Future.successful(Failure(new Exception)))
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val mrn                  = "mrn"
      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val mrnAllocated =
        <CC028A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>{mrn}</DocNumHEA5>
          </HEAHEA>
        </CC028A>
      val result = saveMessageService
        .validateXmlSaveMessageUpdateMrn(mrnAllocated, messageSender, MrnAllocatedResponse, DepartureStatus.MrnAllocated, MovementReferenceNumber(mrn))
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureInternal
      verify(mockDepartureRepository, times(1)).setMrnAndAddResponseMessage(eqTo(departureId),
                                                                            any(),
                                                                            eqTo(DepartureStatus.MrnAllocated),
                                                                            eqTo(MovementReferenceNumber(mrn)))
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

    "return Failure when we cannot parse the message" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Failure(new Exception))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val mrn                  = "mrn"
      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)

      val requestInvalidXmlBody = <Invalid> invalid </Invalid>

      val result = saveMessageService
        .validateXmlSaveMessageUpdateMrn(requestInvalidXmlBody,
                                         messageSender,
                                         DepartureRejectedResponse,
                                         DepartureStatus.DepartureRejected,
                                         MovementReferenceNumber(mrn))
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureExternal
      verify(mockDepartureRepository, never()).addResponseMessage(any(), any(), any())
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

    "return Failure when we cannot parse the message due malformed time" in {
      when(mockXmlValidationService.validate(any(), any())).thenReturn(Success(()))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[XmlValidationService].toInstance(mockXmlValidationService)
        )
        .build()

      val saveMessageService = application.injector.instanceOf[SaveMessageService]

      val mrn                  = "mrn"
      val departureId          = DepartureId(1)
      val messageCorrelationId = 1
      val messageSender        = MessageSender(departureId, messageCorrelationId)
      val dateOfPrep           = LocalDate.now()
      val timeOfPrep           = LocalTime.of(1, 1)

      val mrnAllocated =
        <CC028A>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) + "/"}</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>{mrn}</DocNumHEA5>
          </HEAHEA>
        </CC028A>

      val result = saveMessageService
        .validateXmlSaveMessageUpdateMrn(mrnAllocated, messageSender, MrnAllocatedResponse, DepartureStatus.MrnAllocated, MovementReferenceNumber(mrn))
        .futureValue

      result mustBe SubmissionProcessingResult.SubmissionFailureExternal
      verify(mockDepartureRepository, never()).addResponseMessage(any(), any(), any())
      verify(mockXmlValidationService, times(1)).validate(any(), any())
    }

  }
}
