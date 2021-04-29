package services

import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import models.Box
import connectors.PushPullNotificationConnector
import javax.inject.Inject
import uk.gov.hmrc.http.UpstreamErrorResponse
import logging.Logging
import play.api.http.Status._

class PushPullNotificationService @Inject() (connector: PushPullNotificationConnector) extends Logging {

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
