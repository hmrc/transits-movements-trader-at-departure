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

package controllers

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ChannelType.Web
import models._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.MovementMessageOrchestratorService
import utils.TestMetrics

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class NCTSMessageControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach {

  class Setup {

    protected val mockMovementMessageOrchestratorService: MovementMessageOrchestratorService = mock[MovementMessageOrchestratorService]
    protected val testMetrics: TestMetrics                                                   = new TestMetrics
    protected val messageSender: MessageSender                                               = MessageSender(DepartureId(1), 1)

    protected val initialDeparture: Departure = Departure(
      departureId = DepartureId(1),
      channel = Web,
      movementReferenceNumber = None,
      referenceNumber = "SomeREf",
      eoriNumber = "AB123456",
      created = LocalDateTime.of(2021, 2, 2, 2, 2),
      lastUpdated = LocalDateTime.of(2021, 2, 2, 4, 2),
      messages = NonEmptyList.one(
        MessageWithStatus(
          MessageId(1),
          LocalDateTime.of(2021, 2, 2, 2, 2),
          Some(LocalDateTime.of(2021, 2, 2, 2, 2)),
          MessageType.DepartureDeclaration,
          <CC015></CC015>,
          MessageStatus.SubmissionPending,
          1
        )
      ),
      nextMessageCorrelationId = 2,
      notificationBox = None
    )

    protected val requestMrnAllocatedBody =
      <CC028A>
        <HEAHEA>
          <DocNumHEA5>mrn</DocNumHEA5>
        </HEAHEA>
      </CC028A>

    lazy val controller = new NCTSMessageController(
      stubControllerComponents(),
      mockMovementMessageOrchestratorService,
      testMetrics
    )(ExecutionContext.global)

  }

  "post" - {

    "when a lock can be acquired" - {

      "must return OK, when the service validates and save the message" in new Setup {
        when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
          .thenReturn(Future.successful(Right(SubmissionSuccess(initialDeparture))))

        val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

        status(result) mustEqual OK
        header(LOCATION, result) mustBe Some(
          routes.MessagesController.getMessage(messageSender.departureId, MessageId(messageSender.messageCorrelationId + 1)).url
        )
        contentAsString(result) mustBe empty

        verify(mockMovementMessageOrchestratorService).saveNCTSMessage(eqTo(messageSender))(any(), any())
      }

      "must return BadRequest, when the valid message is out of sequence" in new Setup {
        when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
          .thenReturn(
            Future.successful(
              Left(
                TransitionError(
                  "Can only accept this type of message [CancellationDecision] directly after [DeclarationCancellationRequest or ReleaseForTransit] messages. Current message state is [PositiveAcknowledgement]."
                )
              )
            )
          )

        val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

        contentAsString(
          result
        ) mustEqual "Can only accept this type of message [CancellationDecision] directly after [DeclarationCancellationRequest or ReleaseForTransit] messages. Current message state is [PositiveAcknowledgement]."

        status(result) mustEqual BAD_REQUEST
      }
    }

    "must return BadRequest, when the service is unable to find a mrn before attempting to save the message" in new Setup {

      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(XMLMRNError("The element 'DocNumHEA5' must contain a value."))))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      contentAsString(result) mustEqual "The element 'DocNumHEA5' must contain a value."
      status(result) mustEqual BAD_REQUEST
    }

    "must return BadRequest, when the service is unable to save the message with an external error" in new Setup {
      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(SubmissionFailureExternal)))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      status(result) mustEqual BAD_REQUEST
    }

    "must return InternalServerError, when the service is unable to save the message with an internal error" in new Setup {
      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(SubmissionFailureInternal)))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      status(result) mustEqual INTERNAL_SERVER_ERROR
    }

    "must lock, not update the departure repository, return Ok and unlock when given a message for a departure that does not exist" in new Setup {
      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(DepartureNotFound("Not found"))))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      contentAsString(result) mustBe empty
      status(result) mustEqual NOT_FOUND
    }

    "must lock the departure, return BadRequest error and unlock when an XMessageType is invalid" in new Setup {
      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(InvalidMessageType("A 'X-Message-Type' header must be defined in the request."))))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      contentAsString(result) mustEqual "A 'X-Message-Type' header must be defined in the request."
      status(result) mustEqual BAD_REQUEST
    }

  }

  "when a lock cannot be acquired" - {
    "must return Locked" in new Setup {
      when(mockMovementMessageOrchestratorService.saveNCTSMessage(eqTo(messageSender))(any(), any()))
        .thenReturn(Future.successful(Left(DepartureAlreadyLocked(messageSender.departureId))))

      val result: Future[Result] = controller.post(messageSender)(FakeRequest().withBody(requestMrnAllocatedBody))

      contentAsString(result) mustBe empty
      status(result) mustEqual LOCKED
    }
  }
}
