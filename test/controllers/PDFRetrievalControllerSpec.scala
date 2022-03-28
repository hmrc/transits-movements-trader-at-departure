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

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import akka.util.ByteString
import base.SpecBase
import cats.data.Ior
import controllers.actions.AuthenticateGetDepartureMessagesForReadActionProvider
import generators.ModelGenerators
import models.ChannelType.Web
import models.DepartureId
import models.EORINumber
import models.EnrolmentId
import models.Message
import models.MessageWithStatus
import models.PdfDocument
import models.request.AuthenticatedRequest
import models.request.DepartureMessagesRequest
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.when
import org.mockito.invocation.InvocationOnMock
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterAll
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.Result
import play.api.test.Helpers.contentAsBytes
import play.api.test.Helpers.defaultAwaitTimeout
import play.api.test.Helpers.status
import play.api.test.Helpers.stubControllerComponents
import services.IncorrectStateError
import services.PDFRetrievalService
import services.UnexpectedError
import utils.TestMetrics

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PDFRetrievalControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with BeforeAndAfterAll {
  implicit val system = ActorSystem(suiteName)

  override protected def afterAll(): Unit =
    TestKit.shutdownActorSystem(system)

  class Setup(departureId: DepartureId) {

    val mockPDFGenerationService: PDFRetrievalService                     = mock[PDFRetrievalService]
    val mockAction: AuthenticateGetDepartureMessagesForReadActionProvider = mock[AuthenticateGetDepartureMessagesForReadActionProvider]

    val controller: PDFRetrievalController = new PDFRetrievalController(mockPDFGenerationService, mockAction, stubControllerComponents(), new TestMetrics)

    private val fakeActionBuilder: ActionBuilder[DepartureMessagesRequest, AnyContent] = mock[ActionBuilder[DepartureMessagesRequest, AnyContent]]

    when(mockAction(any(), any())).thenReturn(fakeActionBuilder)

    val message: Message = arbitrary[MessageWithStatus].sample.value

    when(fakeActionBuilder.async(any[DepartureMessagesRequest[AnyContent] => Future[Result]])).thenAnswer {
      (invocation: InvocationOnMock) =>
        val body = invocation
          .getArgument(0)
          .asInstanceOf[DepartureMessagesRequest[AnyContent] => Future[Result]](
            DepartureMessagesRequest(AuthenticatedRequest(fakeRequest, Web, EnrolmentId(Ior.right(EORINumber("eori")))), departureId, Web, List(message))
          )

        stubControllerComponents().actionBuilder.async(body)
    }
  }

  "PDFRetrievalController" - {
    "getTransitAccompanyingDocument" - {

      "should return a 200 if there is data found" in new Setup(DepartureId(23)) {
        when(mockPDFGenerationService.getAccompanyingDocumentPDF(eqTo(message))(any()))
          .thenReturn(Future.successful(Right(PdfDocument(Source.single(ByteString("Hello".getBytes())), None, None, None))))

        val result: Future[Result] = controller.getAccompanyingDocument(DepartureId(23)).apply(fakeRequest)

        status(result) mustBe 200
        contentAsBytes(result) mustBe "Hello".getBytes()
      }

      "should return a conflict if there is incorrect state error" in new Setup(DepartureId(24)) {
        when(mockPDFGenerationService.getAccompanyingDocumentPDF(eqTo(message))(any()))
          .thenReturn(Future.successful(Left(IncorrectStateError)))

        val result: Future[Result] = controller.getAccompanyingDocument(DepartureId(24)).apply(fakeRequest)

        status(result) mustBe 409
      }

      "should return an internal server error if there is unexpected error" in new Setup(DepartureId(25)) {
        when(mockPDFGenerationService.getAccompanyingDocumentPDF(eqTo(message))(any()))
          .thenReturn(Future.successful(Left(UnexpectedError)))

        val result: Future[Result] = controller.getAccompanyingDocument(DepartureId(25)).apply(fakeRequest)

        status(result) mustBe 502
      }
    }
  }

}
