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

import audit.AuditService
import base.SpecBase
import cats.data.NonEmptyList
import config.Constants
import generators.ModelGenerators
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import models._
import models.ChannelType.api
import org.mockito.ArgumentMatchers._
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.DepartureRepository
import repositories.LockRepository
import scala.concurrent.Future
import services.PushPullNotificationService
import services.SaveMessageService
import utils.Format

class NCTSMessageControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  private val mockDepartureRepository: DepartureRepository                 = mock[DepartureRepository]
  private val mockLockRepository: LockRepository                           = mock[LockRepository]
  private val mockSaveMessageService: SaveMessageService                   = mock[SaveMessageService]
  private val mockAuditService: AuditService                               = mock[AuditService]
  private val mockPushPullNotificationService: PushPullNotificationService = mock[PushPullNotificationService]

  private val dateOfPrep = LocalDate.now()
  private val timeOfPrep = LocalTime.of(1, 1)

  private val departureId   = DepartureId(1)
  private val version       = 1
  private val messageSender = MessageSender(departureId, version)
  private val message       = Arbitrary.arbitrary[MessageWithStatus].sample.value

  private val departure = Departure(
    departureId,
    api,
    "eori",
    Some(MovementReferenceNumber("mrn")),
    "ref",
    DepartureStatus.DepartureSubmitted,
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    1,
    NonEmptyList.one(message),
    None
  )

  private val acknowledgedDeparture = Departure(
    departureId,
    api,
    "eori",
    Some(MovementReferenceNumber("mrn")),
    "ref",
    DepartureStatus.PositiveAcknowledgement,
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    LocalDateTime.of(dateOfPrep, timeOfPrep),
    1,
    NonEmptyList.one(message),
    None
  )

  private val testBoxId = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
  private val testBox   = Box(BoxId(testBoxId), Constants.BoxName)

  private val acknowledgedDepartureWithNotificationBox = acknowledgedDeparture.copy(notificationBox = Some(testBox))

  private val requestMrnAllocatedBody =
    <CC028A>
      <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>mrn</DocNumHEA5>
      </HEAHEA>
    </CC028A>

  private val requestCancellationDecisionBody =
    <CC009A>
      <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
      <HEAHEA>
        <DocNumHEA5>mrn</DocNumHEA5>
      </HEAHEA>
    </CC009A>

  private val badRequestMrnAllocatedBody =
    <CC028A>
      <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
      <HEAHEA>
      </HEAHEA>
    </CC028A>

  private val requestDepartureRejectionXmlBody =
    <CC016A>
      <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
      <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
    </CC016A>

  val codeAndXmlBody = Gen.oneOf(
    Seq(
      (MessageType.DeclarationRejected.code, requestDepartureRejectionXmlBody)
    )
  )

  override def beforeEach: Unit = {
    super.beforeEach()
    reset(mockDepartureRepository)
    reset(mockLockRepository)
    reset(mockSaveMessageService)
    reset(mockAuditService)
    reset(mockPushPullNotificationService)
  }

  //TODO: Add tests for the happy path

  "post" - {

    "when a lock can be acquired" - {

      "must return OK, when the service validates and save the message (mrnAllocated)" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          status(result) mustEqual OK
          header(LOCATION, result) mustBe Some(routes.MessagesController.getMessage(departure.departureId, departure.nextMessageId).url)
          contentAsString(result) mustBe empty
          verify(mockAuditService, times(1)).auditNCTSMessages(any(), eqTo(departure.eoriNumber), eqTo(MrnAllocatedResponse), any())(any())
        }
      }

      "must not send push notification when there is no notificationBox present" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[AuditService].toInstance(mockAuditService),
            bind[PushPullNotificationService].toInstance(mockPushPullNotificationService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          status(result) mustEqual OK
          verifyNoInteractions(mockPushPullNotificationService)
        }
      }

      "must send push notification when there is a notificationBox present" in {
        def boxIdMatcher = refEq(testBoxId).asInstanceOf[BoxId]

        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDepartureWithNotificationBox)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))
        when(mockPushPullNotificationService.sendPushNotification(boxIdMatcher, any())(any(), any())).thenReturn(Future.unit)

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[AuditService].toInstance(mockAuditService),
            bind[PushPullNotificationService].toInstance(mockPushPullNotificationService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          status(result) mustEqual OK
          verify(mockPushPullNotificationService, times(1)).sendPushNotification(boxIdMatcher, any())(any(), any())
        }
      }

      "must return BadRequest, when the valid message is out of sequence" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(bind[DepartureRepository].toInstance(mockDepartureRepository), bind[LockRepository].toInstance(mockLockRepository))
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestCancellationDecisionBody)
            .withHeaders("X-Message-Type" -> MessageType.CancellationDecision.code)

          val result = route(application, request).value
          contentAsString(
            result
          ) mustEqual "Can only accept this type of message [CancellationDecision] directly after [DeclarationCancellationRequest or ReleaseForTransit] messages. Current message state is [PositiveAcknowledgement]."
          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest, when the service is unable to find a mrn before attempting to save the message (mrnAllocated)" in {

        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(badRequestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          contentAsString(result) mustEqual "The element 'DocNumHEA5' must contain a value."
          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return BadRequest, when the service is unable to save the message with an external error" in {

        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          status(result) mustEqual BAD_REQUEST
        }
      }

      "must return InternalServerError, when the service is unable to save the message with an internal error" in {

        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(acknowledgedDeparture)))
        when(mockSaveMessageService.validateXmlSaveMessageUpdateMrn(any(), any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          status(result) mustEqual INTERNAL_SERVER_ERROR
        }
      }

      "must return OK, when the service validates and save the message (not MrnAllocated)" in {

        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionSuccess))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService),
            bind[AuditService].toInstance(mockAuditService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestDepartureRejectionXmlBody)
            .withHeaders("X-Message-Type" -> MessageType.DeclarationRejected.code)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual OK
          verify(mockAuditService, times(1)).auditNCTSMessages(any(), eqTo(departure.eoriNumber), eqTo(DepartureRejectedResponse), any())(any())
          header(LOCATION, result) mustBe Some(routes.MessagesController.getMessage(departure.departureId, departure.nextMessageId).url)
        }
      }

      "must lock, return NotFound and unlock when given a message for a departure that does not exist" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(None))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestMrnAllocatedBody)
            .withHeaders("X-Message-Type" -> MessageType.MrnAllocated.code)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual NOT_FOUND
          verify(mockDepartureRepository, never).addResponseMessage(any(), any(), any())
          verify(mockLockRepository, times(1)).lock(departureId)
          verify(mockLockRepository, times(1)).unlock(departureId)
        }
      }

      "must lock, return Internal Server Error and unlock if adding the message to the movement fails" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureInternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestDepartureRejectionXmlBody)
            .withHeaders("X-Message-Type" -> MessageType.DeclarationRejected.code)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual INTERNAL_SERVER_ERROR
          verify(mockLockRepository, times(1)).lock(departureId)
          verify(mockLockRepository, times(1)).unlock(departureId)
        }
      }

      "must lock the departure, return BadRequest error and unlock when an XMessageType is invalid" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestDepartureRejectionXmlBody)

          val result = route(application, request).value

          contentAsString(result) mustEqual "A 'X-Message-Type' header must be defined in the request."
          status(result) mustEqual BAD_REQUEST
          verify(mockLockRepository, times(1)).lock(departureId)
          verify(mockSaveMessageService, never()).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
          verify(mockLockRepository, times(1)).unlock(departureId)
        }
      }

      "must lock the departure, return BadRequest error and unlock when fail to validate message" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))
        when(mockSaveMessageService.validateXmlAndSaveMessage(any(), any(), any(), any(), any()))
          .thenReturn(Future.successful(SubmissionProcessingResult.SubmissionFailureExternal))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(true))
        when(mockLockRepository.unlock(any())).thenReturn(Future.successful(()))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository),
            bind[SaveMessageService].toInstance(mockSaveMessageService)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestDepartureRejectionXmlBody)
            .withHeaders("X-Message-Type" -> MessageType.DeclarationRejected.code)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual BAD_REQUEST
          verify(mockLockRepository, times(1)).lock(departureId)
          verify(mockSaveMessageService, times(1)).validateXmlAndSaveMessage(any(), any(), any(), any(), any())
          verify(mockLockRepository, times(1)).unlock(departureId)
        }
      }

    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        when(mockDepartureRepository.get(any())).thenReturn(Future.successful(Some(departure)))
        when(mockLockRepository.lock(any())).thenReturn(Future.successful(false))

        val application = baseApplicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        running(application) {
          val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withXmlBody(requestDepartureRejectionXmlBody)
            .withHeaders("X-Message-Type" -> MessageType.DeclarationRejected.code)

          val result = route(application, request).value

          contentAsString(result) mustBe empty
          status(result) mustEqual LOCKED
        }
      }
    }
  }
}
