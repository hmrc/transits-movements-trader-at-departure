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
import cats.data.NonEmptyList
import models.ChannelType.Web
import models.ChannelType
import models.Departure
import models.DepartureId
import models.DepartureNotFound
import models.DepartureStatus
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MessageWithStatus
import models.MrnAllocatedResponse
import org.mockito.Mockito.never
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import repositories.DepartureRepository
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import play.api.libs.json.Json

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem

class DepartureRetrievalServiceSpec extends SpecBase {

  class Setup {
    val mockRepo: DepartureRepository  = mock[DepartureRepository]
    val mockAuditService: AuditService = mock[AuditService]

    val departureId: DepartureId = DepartureId(1)

    lazy val service = new DepartureRetrievalService(mockRepo, mockAuditService)

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

  "getDepartureAndAuditDeletedDepartures" - {
    "must retrieve the departure if it exists" in new Setup {
      when(mockRepo.get(eqTo(departureId)))
        .thenReturn(Future.successful(Some(initialDeparture)))

      val node: Elem = <Node></Node>

      service.getDepartureAndAuditDeletedDepartures(departureId, MrnAllocatedResponse, node).value.futureValue mustBe Right(initialDeparture)

      verify(mockRepo).get(eqTo(departureId))
      verify(mockAuditService, never()).auditNCTSMessages(any(), any(), any())(any())
    }

    "must audit the message if its not found" in new Setup {
      when(mockRepo.get(eqTo(departureId)))
        .thenReturn(Future.successful(None))

      val node: Elem = <Node></Node>

      service
        .getDepartureAndAuditDeletedDepartures(departureId, MrnAllocatedResponse, node)
        .value
        .futureValue
        .mustBe(Left(DepartureNotFound(s"[GetDepartureService][getDepartureById] Unable to retrieve departure message for arrival id: ${departureId.index}")))

      verify(mockRepo).get(eqTo(departureId))
      verify(mockAuditService).auditNCTSMessages(eqTo(ChannelType.Deleted), eqTo(MrnAllocatedResponse), eqTo(node))(any())
    }
  }
}
