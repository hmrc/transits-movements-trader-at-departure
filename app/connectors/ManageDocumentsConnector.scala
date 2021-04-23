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

package connectors

import javax.inject.Inject

import akka.util.ByteString
import com.kenshoo.play.metrics.Metrics
import config.AppConfig
import logging.Logging
import metrics.HasMetrics
import play.api.libs.ws.WSClient
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class ManageDocumentsConnector @Inject()(
  config: AppConfig,
  ws: WSClient,
  val metrics: Metrics
)(implicit val ec: ExecutionContext)
    extends Logging
    with HasMetrics {

  def getTadPDF(ie29Message: NodeSeq)(implicit hc: HeaderCarrier): Future[Either[TADErrorResponse, ByteString]] =
    withMetricsTimerAsync("get-tad-pdf") {
      timer =>
        val serviceUrl = s"${config.manageDocumentsUrl}/transit-accompanying-document"
        val headers = Seq(
          "Content-Type" -> "application/xml",
          "User-Agent"   -> config.appName
        ) ++ hc.headers

        ws.url(serviceUrl)
          .withHttpHeaders(headers: _*)
          .post(ie29Message)
          .map(
            response =>
              if (response.status == 200) {
                timer.completeWithSuccess()
                Right(response.bodyAsBytes)
              } else {
                logger.warn(s"[getTADPdf] returned an unexpected status (${response.status}) while trying to retrieve the TAD")
                timer.completeWithFailure()
                Left(UnexpectedResponse(response.status))
            }
          )
    }

  def getTsadPDF(ie29Message: NodeSeq)(implicit hc: HeaderCarrier): Future[Either[TADErrorResponse, ByteString]] =
    withMetricsTimerAsync("get-tsad-pdf") {
      timer =>
        val serviceUrl = s"${config.manageDocumentsUrl}/transit-security-accompanying-document"
        val headers = Seq(
          "Content-Type" -> "application/xml",
          "User-Agent"   -> config.appName
        ) ++ hc.headers

        ws.url(serviceUrl)
          .withHttpHeaders(headers: _*)
          .post(ie29Message)
          .map(
            response =>
              if (response.status == 200) {
                timer.completeWithSuccess()
                Right(response.bodyAsBytes)
              } else {
                logger.warn(s"[getTsadPDF] returned an unexpected status (${response.status}) while trying to retrieve the TAD")
                timer.completeWithFailure()
                Left(UnexpectedResponse(response.status))
            }
          )
    }

}

sealed trait TADErrorResponse

case class UnexpectedResponse(code: Int) extends TADErrorResponse