package connectors

import com.google.inject.Inject
import config.AppConfig
import config.Constants
import models.Box
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PushPullNotificationConnector @Inject() (config: AppConfig, http: HttpClient) {

  def getBox(clientId: String)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Either[UpstreamErrorResponse, Box]] = {
    val queryParams = Seq(
      "boxName"  -> Constants.BoxName,
      "clientId" -> clientId
    )
    val url = s"${config.pushPullUrl}/box"

    http.GET[Either[UpstreamErrorResponse, Box]](url, queryParams)
  }
}
