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

package services

import audit.AuditService
import base.SpecBase
import cats.data.EitherT
import cats.data.NonEmptyList
import com.kenshoo.play.metrics.Metrics
import models.ChannelType.Api
import models.ChannelType.Web
import models.BoxId
import models.ChannelType
import models.Departure
import models.DepartureId
import models.DepartureMessageNotification
import models.DepartureNotFound
import models.DepartureStatus
import models.MessageId
import models.MessageResponse
import models.MessageSender
import models.MessageStatus
import models.MessageType
import models.MessageWithStatus
import models.MrnAllocatedResponse
import models.ReleaseForTransitResponse
import models.SubmissionFailureExternal
import models.SubmissionFailureInternal
import models.SubmissionProcessingResult
import models.SubmissionState
import models.SubmissionSuccess
import models.TransitionError
import models.XMLMRNError
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.never
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.verifyNoInteractions
import org.mockito.Mockito.when
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import play.api.libs.json.Json
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MovementMessageOrchestratorServiceSpec extends SpecBase {

  class Setup {
    case object MockLockService extends LockService(null)(ExecutionContext.global) {
      override def withLock[T](departureId: DepartureId)(action: => EitherT[Future, SubmissionState, T]): EitherT[Future, SubmissionState, T] = action
    }

    val mockLockService: LockService                       = MockLockService
    val mockGetDepartureService: DepartureRetrievalService = mock[DepartureRetrievalService]
    val mockSaveMessageService: SaveMessageService         = mock[SaveMessageService]
    val mockAuditService: AuditService                     = mock[AuditService]
    val mockPullPushService: PushPullNotificationService   = mock[PushPullNotificationService]
    val mockMetrics: Metrics                               = mock[Metrics]

    lazy val service = new MovementMessageOrchestratorService(
      mockLockService,
      mockGetDepartureService,
      mockSaveMessageService,
      mockAuditService,
      mockPullPushService,
      mockMetrics
    )(ExecutionContext.global)

    protected val initialDeparture: Departure = Departure(
      departureId = DepartureId(1),
      channel = Web,
      movementReferenceNumber = None,
      referenceNumber = "SomeREf",
      eoriNumber = "AB123456",
      status = DepartureStatus.DepartureSubmitted,
      created = LocalDateTime.of(2021, 2, 2, 2, 2),
      lastUpdated = LocalDateTime.of(2021, 2, 2, 4, 2),
      messages = NonEmptyList.one(
        MessageWithStatus(
          MessageId(1),
          LocalDateTime.of(2021, 2, 2, 2, 2),
          MessageType.DepartureDeclaration,
          <CC015></CC015>,
          MessageStatus.SubmissionPending,
          1,
          Json.obj("CC029" -> Json.obj())
        )
      ),
      nextMessageCorrelationId = 2,
      notificationBox = None
    )
  }

  "saveNCTSMessage" - {
    "must successfully save the message updating the mrn if it is mrn allocated" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC015>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC015>)
        .withHeaders("X-Message-Type" -> "IE028")

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](initialDeparture))))

      when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Right(SubmissionSuccess(initialDeparture))

      verify(mockSaveMessageService).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(MrnAllocatedResponse), any())(any())
      verify(mockPullPushService).sendPushNotificationIfBoxExists(any(), any(), any())(any(), any())
    }

    "must successfully save the message" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC029>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC029>)
        .withHeaders("X-Message-Type" -> "IE029")

      val departure: Departure = initialDeparture.copy(status = DepartureStatus.MrnAllocated)

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](departure))))

      when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Right(SubmissionSuccess(departure))

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(ReleaseForTransitResponse), any())(any())
      verify(mockPullPushService).sendPushNotificationIfBoxExists(any(), any(), any())(any(), any())
    }

    "must return an invalid transition message for an invalid transition" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC029>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC029>)
        .withHeaders("X-Message-Type" -> "IE029")

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](initialDeparture))))

      service
        .saveNCTSMessage(MessageSender(DepartureId(1), 1))
        .futureValue
        .mustBe(Left(TransitionError(
          "Can only accept this type of message [ReleaseForTransit] directly after [ControlDecisionNotification or DeclarationCancellationRequest or GuaranteeNotValid or MrnAllocated] messages. Current message state is [DepartureSubmitted].")))

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService, never()).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(ReleaseForTransitResponse), any())(any())
      verifyNoInteractions(mockPullPushService)
    }

    "must return an Departure Not found when no departure is found" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC029>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC029>)
        .withHeaders("X-Message-Type" -> "IE029")

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Left[SubmissionState, Departure](DepartureNotFound("")))))

      when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Left(DepartureNotFound(""))

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService, never()).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(ReleaseForTransitResponse), any())(any())
      verifyNoInteractions(mockPullPushService)
    }

    "must return a SubmissionFailureInternal if message cannot be saved for internal errors" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC029>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC029>)
        .withHeaders("X-Message-Type" -> "IE029")

      val departure: Departure = initialDeparture.copy(status = DepartureStatus.MrnAllocated)

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](departure))))

      when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Left(SubmissionFailureInternal)

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService, never()).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(ReleaseForTransitResponse), any())(any())
      verifyNoInteractions(mockPullPushService)
    }

    "must return a SubmissionFailureExternal if message cannot be saved for external errors" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC029>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>234444</DocNumHEA5>
          </HEAHEA>
        </CC029>)
        .withHeaders("X-Message-Type" -> "IE029")

      val departure: Departure = initialDeparture.copy(status = DepartureStatus.MrnAllocated)

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](departure))))

      when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Left(SubmissionFailureExternal)

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService, never()).auditNCTSMessages(eqTo(ChannelType.Web), eqTo(ReleaseForTransitResponse), any())(any())
      verifyNoInteractions(mockPullPushService)
    }

    "must return a Left mrn not found if trying to submit an mrn allocated message" in new Setup {
      implicit val request: Request[NodeSeq] = FakeRequest()
        .withBody(<CC015>
          <DatOfPreMES9>20201212</DatOfPreMES9>
          <TimOfPreMES10>1220</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA>234444</DocNumHEA>
          </HEAHEA>
        </CC015>)
        .withHeaders("X-Message-Type" -> "IE028")

      when(mockGetDepartureService.getDepartureAndAuditDeletedDepartures(any(), any(), any())(any()))
        .thenReturn(EitherT[Future, SubmissionState, Departure](Future.successful(Right[SubmissionState, Departure](initialDeparture))))

      when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))

      service.saveNCTSMessage(MessageSender(DepartureId(1), 1)).futureValue mustBe Left(XMLMRNError("The element 'DocNumHEA5' must contain a value."))

      verify(mockSaveMessageService, never()).validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any())
      verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
      verify(mockAuditService, never()).auditNCTSMessages(any(), any(), any())(any())
      verifyNoInteractions(mockPullPushService)
    }
  }
}
