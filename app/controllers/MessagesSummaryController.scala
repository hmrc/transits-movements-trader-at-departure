/*
 * Copyright 2023 HM Revenue & Customs
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
import controllers.actions.AuthenticatedGetDepartureWithMessagesForReadActionProvider
import metrics.HasActionMetrics
import models.DepartureId
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.MessageSummaryService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext

class MessagesSummaryController @Inject()(
  authenticateForRead: AuthenticatedGetDepartureWithMessagesForReadActionProvider,
  messageSummaryService: MessageSummaryService,
  cc: ControllerComponents,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with HasActionMetrics {

  def messagesSummary(departureId: DepartureId): Action[AnyContent] =
    withMetricsTimerAction("get-departure-message-summary") {
      authenticateForRead(departureId) {
        implicit request =>
          val messageSummary = messageSummaryService.messagesSummary(request.departure)
          Ok(Json.toJsObject(messageSummary))
      }
    }
}
