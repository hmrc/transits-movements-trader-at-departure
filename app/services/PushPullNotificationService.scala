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

import connectors.PushPullNotificationConnector
import models.Box
import models.BoxId
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.UpstreamErrorResponse

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import scala.util.control.NonFatal

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
      .recover {
        case NonFatal(e) =>
          logger.error(s"Error while fetching notification box", e)
          None
      }

  def sendPushNotification(boxId: BoxId, body: NodeSeq)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Unit] =
    connector
      .postNotification(boxId, body)
      .map {
        case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
          logger.warn(s"Error $statusCode received while sending notification for boxId $boxId: $message")
        case Right(_) => ()

      }
      .recover {
        case NonFatal(e) =>
          logger.error(s"Error while sending push notification", e)
      }

}
