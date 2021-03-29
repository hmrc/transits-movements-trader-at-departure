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

import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import models.DepartureId
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.IncorrectStateError
import services.PDFRetrievalService
import services.UnexpectedError
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.Logging

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class PDFRetrievalController @Inject()(
  pdfGenerationService: PDFRetrievalService,
  authenticateForRead: AuthenticatedGetDepartureForReadActionProvider,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  def getTransitAccompanyingDocument(departureId: DepartureId): Action[AnyContent] = authenticateForRead(departureId).async {
    implicit request =>
      pdfGenerationService.getAccompanyingDocumentPDF(request.departure).map {
        case Right(response) => Ok(response)
        case Left(UnexpectedError) =>
          logger.warn(s"[getTransitAccompanyingDocument] returning $BAD_GATEWAY due to an unexpected error in getting the PDF")
          BadGateway
        case Left(IncorrectStateError) =>
          logger.warn(s"[getTransitAccompanyingDocument] returning $CONFLICT due to an IncorrectStateError in getting the PDF")
          Conflict
      }
  }
}
