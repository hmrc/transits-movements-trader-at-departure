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

import cats.data.EitherT
import connectors.ManageDocumentsConnector
import models.Departure
import models.PdfDocument
import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.xml.NodeSeq

class PDFRetrievalService @Inject()(
  manageDocumentsConnector: ManageDocumentsConnector,
  messageRetrievalService: MessageRetrievalService
)(implicit val ec: ExecutionContext)
    extends Logging {

  private def messageIsSafety(message: NodeSeq): Boolean = (message \ "HEAHEA" \ "SecHEA358").headOption.exists(_.text == "1")

  def getAccompanyingDocumentPDF(departure: Departure)(implicit hc: HeaderCarrier): Future[Either[PDFGenerationResponse, PdfDocument]] = {
    val pdfResponse = messageRetrievalService.getReleaseForTransitMessage(departure) match {
      case Some(ie29Message) if messageIsSafety(ie29Message.message) =>
        val pdfResponse = EitherT(manageDocumentsConnector.getTsadPDF(ie29Message.message))
        pdfResponse.leftMap(_ => UnexpectedError).value
      case Some(ie29Message) =>
        val pdfResponse = EitherT(manageDocumentsConnector.getTadPDF(ie29Message.message))
        pdfResponse.leftMap(_ => UnexpectedError).value
      case _ =>
        logger.warn("[getAccompanyingDocumentPDF] no releaseForTransitMessageId or Departure found")
        Future.successful(Left(IncorrectStateError))
    }

    pdfResponse.recover {
      case NonFatal(e) =>
        logger.error("[getAccompanyingDocumentPDF] an unexpected exception happened", e)
        Left(UnexpectedError)
    }
  }
}

sealed trait PDFGenerationResponse
case object UnexpectedError     extends PDFGenerationResponse
case object IncorrectStateError extends PDFGenerationResponse
