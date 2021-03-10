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
import connectors.UnexpectedResponse
import models.Departure
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logging

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PDFRetrievalService @Inject()(
  manageDocumentsConnector: ManageDocumentsConnector,
  messageRetrievalService: MessageRetrievalService
)(implicit val ec: ExecutionContext)
    extends Logging {

  def getTadPDF(departure: Departure)(implicit hc: HeaderCarrier): Future[Either[PDFGenerationResponse, ByteString]] =
    messageRetrievalService.getReleaseForTransitMessage(departure) match {
      case Some(ie29Message) =>
        manageDocumentsConnector
          .getTadPDF(ie29Message.message)
          .map {
            case Right(pdf)                  => Right(pdf)
            case Left(UnexpectedResponse(_)) => Left(UnexpectedError)
          }
          .recover {
            case e =>
              logger.error("[getTadPDF] an unexpected exception happened", e)
              Left(UnexpectedError)
          }
      case _ =>
        logger.warn("[getTadPDF] no releaseForTransitMessageId or Departure found")
        Future.successful(Left(IncorrectStateError))
    }
}

sealed trait PDFGenerationResponse
case object UnexpectedError     extends PDFGenerationResponse
case object IncorrectStateError extends PDFGenerationResponse
