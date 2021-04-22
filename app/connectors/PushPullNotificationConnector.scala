package connectors

import com.google.inject.Inject
import config.AppConfig
import models.Box
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.{ExecutionContext, Future}

class PushPullNotificationConnector @Inject()(config: AppConfig, http: HttpClient)(implicit ec: ExecutionContext, hc: HeaderCarrier) {
  def getBox(boxName: String, clientId: String): Future[Option[Box]] = {
    val queryParams = Seq(
      "boxName" -> boxName,
      "clientId" -> clientId
    )

    http.GET[Option[Box]](???, queryParams)
  }
}
