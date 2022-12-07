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

package services

import connectors.PushPullNotificationConnector
import models.Box
import models.BoxId
import models.Departure
import models.DepartureMessageNotification
import models.MessageResponse
import play.api.Logging
import play.api.http.Status._
import play.api.mvc.Request
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.UpstreamErrorResponse

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.xml.NodeSeq

class PushPullNotificationService @Inject()(connector: PushPullNotificationConnector)(implicit ec: ExecutionContext) extends Logging {

  def getBox(clientId: String)(implicit hc: HeaderCarrier): Future[Option[Box]] =
    connector
      .getBox(clientId)
      .map {
        case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
          if (statusCode != NOT_FOUND) logger.warn(s"Unable to retrieve box id from PPNS. Response from PPNS: Error $statusCode $message")
          None
        case Right(box) => Some(box)
      }
      .recover {
        case NonFatal(e) =>
          logger.error("Unable to retrieve box id from PPNS", e)
          None
      }

  def sendPushNotification(boxId: BoxId, notification: DepartureMessageNotification)(implicit hc: HeaderCarrier): Future[Unit] =
    connector
      .postNotification(boxId, notification)
      .flatMap {
        case Left(UpstreamErrorResponse(_, REQUEST_ENTITY_TOO_LARGE, _, _)) if notification.messageBody.isDefined =>
          connector.postNotification(boxId, notification.copy(messageBody = None))
        case x => Future.successful(x)
      }
      .map {
        case Left(UpstreamErrorResponse(message, statusCode, _, _)) =>
          logger.warn(s"Unable to post message to PPNS. Response from PPNS: Error $statusCode $message")
        case Right(_) => ()
      }
      .recover {
        case NonFatal(e) =>
          logger.error("Unable to post message to PPNS", e)
      }

  def sendPushNotificationIfBoxExists(
    departure: Departure,
    messageResponse: MessageResponse,
    request: Request[NodeSeq]
  )(implicit hc: HeaderCarrier): Future[Unit] =
    departure.notificationBox
      .map {
        box =>
          XmlMessageParser.dateTimeOfPrepR(request.body) match {
            case Left(error) =>
              logger.error(s"Error while parsing message timestamp: ${error.message}")
              Future.unit
            case Right(timestamp) =>
              val notification = DepartureMessageNotification.fromDepartureAndResponse(departure, messageResponse, timestamp, request)
              sendPushNotification(box.boxId, notification)
          }
      }
      .getOrElse(Future.unit)

}
