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

import javax.inject.Inject

import com.kenshoo.play.metrics.Metrics
import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import logging.Logging
import metrics.HasActionMetrics
import models.DepartureId
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.IncorrectStateError
import services.PDFRetrievalService
import services.UnexpectedError
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext

class PDFRetrievalController @Inject()(
  pdfGenerationService: PDFRetrievalService,
  authenticateForRead: AuthenticatedGetDepartureForReadActionProvider,
  cc: ControllerComponents,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  def getAccompanyingDocument(departureId: DepartureId): Action[AnyContent] =
    //withMetricsTimerAction("get-accompanying-document") {
    authenticateForRead(departureId).async {
      implicit request =>
        pdfGenerationService.getAccompanyingDocumentPDF(request.departure).map {
          case Right(response) => Ok(response)
          case Left(UnexpectedError) =>
            logger.warn(s"[getAccompanyingDocument] returning $BAD_GATEWAY due to an unexpected error in getting the PDF")
            BadGateway
          case Left(IncorrectStateError) =>
            logger.warn(s"[getAccompanyingDocument] returning $CONFLICT due to an IncorrectStateError in getting the PDF")
            Conflict
        }
    }
  //}
}
