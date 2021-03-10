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

import akka.util.ByteString
import base.SpecBase
import cats.data.NonEmptyList
import connectors.ManageDocumentsConnector
import models.ChannelType
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageType
import models.MessageWithoutStatus
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.when
import org.scalatest.concurrent.IntegrationPatience
import uk.gov.hmrc.http.NotFoundException

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PDFRetrievalServiceSpec extends SpecBase with IntegrationPatience {

  val mockManageDocumentsConnector: ManageDocumentsConnector = mock[ManageDocumentsConnector]
  val mockMessageRetrievalService: MessageRetrievalService   = mock[MessageRetrievalService]
  lazy val service                                           = new PDFRetrievalService(mockManageDocumentsConnector, mockMessageRetrievalService)

  "PDFRetrievalService" - {
    val departure = Departure(
      DepartureId(1),
      ChannelType.web,
      "AB123456C",
      None,
      "SomeReference",
      DepartureStatus.ReleaseForTransit,
      LocalDateTime.now(),
      LocalDateTime.now(),
      2,
      NonEmptyList(MessageWithoutStatus(LocalDateTime.now(), MessageType.DepartureDeclaration, <node></node>, 1), Nil)
    )

    "getTadPDF" - {
      "should return the WSResponse if all messages found and returned from manage documents" in {
        when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
          .thenReturn(Some(MessageWithoutStatus(LocalDateTime.now, MessageType.ReleaseForTransit, <blank2></blank2>, 2)))

        when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank2></blank2>))(any()))
          .thenReturn(Future.successful(Right(ByteString("Hello".getBytes()))))

        service.getTadPDF(departure).futureValue mustBe Right(ByteString("Hello".getBytes()))
      }

      "should return an UnexpectedError an unexpected response PDF" in {
        when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
          .thenReturn(Some(MessageWithoutStatus(LocalDateTime.now, MessageType.ReleaseForTransit, <blank1></blank1>, 2)))

        when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
          .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

        service.getTadPDF(departure).futureValue mustBe Left(UnexpectedError)
      }

      "should return an UnexpectedError if there is a failure in retrieving the PDF" in {
        when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
          .thenReturn(Some(MessageWithoutStatus(LocalDateTime.now, MessageType.ReleaseForTransit, <blank1></blank1>, 2)))

        when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
          .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

        service.getTadPDF(departure).futureValue mustBe Left(UnexpectedError)
      }

      "should return an IncorrectStateError if there no TAD pdf request can be generated" in {
        when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
          .thenReturn(None)

        service.getTadPDF(departure).futureValue.left.value mustBe IncorrectStateError
      }
    }
  }
}
