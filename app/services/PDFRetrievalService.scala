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
import connectors.ManageDocumentsConnector
import connectors.TADErrorResponse
import connectors.UnexpectedResponse
import models.Departure
import uk.gov.hmrc.http.HeaderCarrier
import javax.inject.Inject
import play.api.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class PDFRetrievalService @Inject()(
  manageDocumentsConnector: ManageDocumentsConnector,
  messageRetrievalService: MessageRetrievalService
)(implicit val ec: ExecutionContext)
    extends Logging {

  private def responseHandler(functionName: String)(
    f: => Future[Either[TADErrorResponse, ByteString]]
  ): Future[Either[PDFGenerationResponse, ByteString]] =
    f.map {
        case Right(pdf)                  => Right(pdf)
        case Left(UnexpectedResponse(_)) => Left(UnexpectedError)
      }
      .recover {
        case e =>
          logger.error(s"[getAccompanyingDocumentPDF][$functionName] an unexpected exception happened", e)
          Left(UnexpectedError)
      }

  private def messageIsSafety(message: NodeSeq): Boolean = (message \ "HEAHEA" \ "SecHEA358").headOption.exists(_.text == "1")

  def getAccompanyingDocumentPDF(departure: Departure)(implicit hc: HeaderCarrier): Future[Either[PDFGenerationResponse, ByteString]] =
    messageRetrievalService.getReleaseForTransitMessage(departure) match {
      case Some(ie29Message) if messageIsSafety(ie29Message.message) =>
        responseHandler("TSAD")(manageDocumentsConnector.getTsadPDF(ie29Message.message))
      case Some(ie29Message) =>
        responseHandler("TAD")(manageDocumentsConnector.getTadPDF(ie29Message.message))
      case _ =>
        logger.warn("[getAccompanyingDocumentPDF] no releaseForTransitMessageId or Departure found")
        Future.successful(Left(IncorrectStateError))
    }
}

sealed trait PDFGenerationResponse
case object UnexpectedError     extends PDFGenerationResponse
case object IncorrectStateError extends PDFGenerationResponse
