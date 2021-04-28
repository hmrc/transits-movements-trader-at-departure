package services

import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import models.Box
import connectors.PushPullNotificationConnector
import javax.inject.Inject

class PushPullNotificationService @Inject() (connector: PushPullNotificationConnector) {

  def getBox(clientId: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Option[Box]] =
    connector.getBox(clientId).map(_.toOption)

}
