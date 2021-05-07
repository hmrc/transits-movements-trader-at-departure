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

import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import models.Box
import connectors.PushPullNotificationConnector
import javax.inject.Inject
import uk.gov.hmrc.http.UpstreamErrorResponse
import play.api.Logging
import play.api.http.Status._

class PushPullNotificationService @Inject()(connector: PushPullNotificationConnector) extends Logging {

  def getBox(clientId: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Option[Box]] =
    connector
      .getBox(clientId)
      .map {
        case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
          if (statusCode != NOT_FOUND) logger.warn(s"Error $statusCode received while fetching notification box: $message")
          None
        case Right(box) => Some(box)
      }

}
