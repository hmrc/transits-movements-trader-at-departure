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

import akka.stream.scaladsl.Source
import akka.util.ByteString
import base.SpecBase
import cats.data.NonEmptyList
import connectors.ManageDocumentsConnector
import models.ChannelType
import models.Departure
import models.DepartureId
import models.MessageId
import models.MessageType
import models.MessageWithoutStatus
import models.PdfDocument
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import uk.gov.hmrc.http.NotFoundException
import utils.JsonHelper

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PDFRetrievalServiceSpec extends SpecBase with JsonHelper with IntegrationPatience with BeforeAndAfterEach {

  val mockManageDocumentsConnector: ManageDocumentsConnector = mock[ManageDocumentsConnector]
  val mockMessageRetrievalService: MessageRetrievalService   = mock[MessageRetrievalService]
  lazy val service                                           = new PDFRetrievalService(mockManageDocumentsConnector, mockMessageRetrievalService)

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockManageDocumentsConnector, mockMessageRetrievalService)
  }

  "PDFRetrievalService" - {

    def safetyXML(value: Int) = <CC029B><HEAHEA><SecHEA358>{value}</SecHEA358></HEAHEA></CC029B>

    "getAccompanyingDocumentPDF(Message)" - {
      "TAD" - {
        "should return PDF with correct headers if all messages found and returned from manage documents where message does not contain safety and security" in {
          val message = MessageWithoutStatus(
            MessageId(2),
            LocalDateTime.now,
            MessageType.ReleaseForTransit,
            <blank2></blank2>,
            2,
            convertXmlToJson(<blank2></blank2>.toString())
          )

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), None, Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank2></blank2>))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank2></blank2>))(any())
        }

        "should return the PDF with correct headers if all messages found and returned from manage documents where safety and security is 0" in {
          val message = MessageWithoutStatus(
            MessageId(2),
            LocalDateTime.now,
            MessageType.ReleaseForTransit,
            safetyXML(0),
            2,
            convertXmlToJson(<blank2></blank2>.toString())
          )

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), None, Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTadPDF(eqTo(safetyXML(0)))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(safetyXML(0)))(any())
        }

        "should return an UnexpectedError an unexpected response PDF" in {
          val message = MessageWithoutStatus(
            MessageId(2),
            LocalDateTime.now,
            MessageType.ReleaseForTransit,
            <blank1></blank1>,
            2,
            convertXmlToJson(<blank1></blank1>.toString())
          )

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank1></blank1>))(any())
        }

        "should return an UnexpectedError if there is a failure in retrieving the PDF" in {
          val message = MessageWithoutStatus(
            MessageId(2),
            LocalDateTime.now,
            MessageType.ReleaseForTransit,
            <blank1></blank1>,
            2,
            convertXmlToJson(<blank1></blank1>.toString())
          )

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank1></blank1>))(any())
        }

      }

      "TSAD" - {
        val xml = safetyXML(1)
        "should return PDF with correct headers if all messages found and returned from manage documents where message contains safety and security" in {
          val message = MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), Some(5L), Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }

        "should return an UnexpectedError an unexpected response PDF" in {
          val message = MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }

        "should return an UnexpectedError if there is a failure in retrieving the PDF" in {
          val message = MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(message).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }
      }
    }

    "getAccompanyingDocumentPDF(Departure)" - {

      val departure = Departure(
        DepartureId(1),
        ChannelType.Web,
        "AB123456C",
        None,
        "SomeReference",
        LocalDateTime.now(),
        LocalDateTime.now(),
        2,
        NonEmptyList(
          MessageWithoutStatus(
            MessageId(1),
            LocalDateTime.now(),
            MessageType.DepartureDeclaration,
            <node></node>,
            1,
            convertXmlToJson(<node></node>.toString())
          ),
          Nil
        ),
        None
      )

      "TAD" - {
        "should return PDF with correct headers if all messages found and returned from manage documents where message does not contain safety and security" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(
              Some(
                MessageWithoutStatus(
                  MessageId(2),
                  LocalDateTime.now,
                  MessageType.ReleaseForTransit,
                  <blank2></blank2>,
                  2,
                  convertXmlToJson(<blank2></blank2>.toString())
                )
              )
            )

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), None, Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank2></blank2>))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank2></blank2>))(any())
        }

        "should return the PDF with correct headers if all messages found and returned from manage documents where safety and security is 0" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(
              Some(
                MessageWithoutStatus(
                  MessageId(2),
                  LocalDateTime.now,
                  MessageType.ReleaseForTransit,
                  safetyXML(0),
                  2,
                  convertXmlToJson(<blank2></blank2>.toString())
                )
              )
            )

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), None, Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTadPDF(eqTo(safetyXML(0)))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(safetyXML(0)))(any())
        }

        "should return an UnexpectedError an unexpected response PDF" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(
              Some(
                MessageWithoutStatus(
                  MessageId(2),
                  LocalDateTime.now,
                  MessageType.ReleaseForTransit,
                  <blank1></blank1>,
                  2,
                  convertXmlToJson(<blank1></blank1>.toString())
                )
              )
            )

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank1></blank1>))(any())
        }

        "should return an UnexpectedError if there is a failure in retrieving the PDF" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(
              Some(
                MessageWithoutStatus(
                  MessageId(2),
                  LocalDateTime.now,
                  MessageType.ReleaseForTransit,
                  <blank1></blank1>,
                  2,
                  convertXmlToJson(<blank1></blank1>.toString())
                )
              )
            )

          when(mockManageDocumentsConnector.getTadPDF(eqTo(<blank1></blank1>))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(1)).getTadPDF(eqTo(<blank1></blank1>))(any())
        }

        "should return an IncorrectStateError if there no TAD pdf request can be generated" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(None)

          service.getAccompanyingDocumentPDF(departure).futureValue.left.value mustBe IncorrectStateError

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())
          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }
      }

      "TSAD" - {
        val xml = safetyXML(1)
        "should return PDF with correct headers if all messages found and returned from manage documents where message contains safety and security" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(Some(MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))))

          val pdfDocument = PdfDocument(Source.single(ByteString("Hello".getBytes())), Some(5L), Some("value"), Some("value"))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any())).thenReturn(Future.successful(Right(pdfDocument)))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Right(pdfDocument)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }

        "should return an UnexpectedError an unexpected response PDF" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(Some(MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }

        "should return an UnexpectedError if there is a failure in retrieving the PDF" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(Some(MessageWithoutStatus(MessageId(2), LocalDateTime.now, MessageType.ReleaseForTransit, xml, 2, convertXmlToJson(xml.toString()))))

          when(mockManageDocumentsConnector.getTsadPDF(eqTo(xml))(any()))
            .thenReturn(Future.failed(new NotFoundException("Sorry An Exception Occurred")))

          service.getAccompanyingDocumentPDF(departure).futureValue mustBe Left(UnexpectedError)

          verify(mockManageDocumentsConnector, times(1))
            .getTsadPDF(eqTo(xml))(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }

        "should return an IncorrectStateError if there no TAD pdf request can be generated" in {
          when(mockMessageRetrievalService.getReleaseForTransitMessage(eqTo(departure)))
            .thenReturn(None)

          service.getAccompanyingDocumentPDF(departure).futureValue.left.value mustBe IncorrectStateError

          verify(mockManageDocumentsConnector, times(0)).getTsadPDF(any())(any())

          verify(mockManageDocumentsConnector, times(0)).getTadPDF(any())(any())
        }
      }
    }
  }
}
