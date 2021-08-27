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

import com.google.inject.Inject
import com.kenshoo.play.metrics.Metrics
import config.AppConfig
import config.Constants
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult._
import metrics.HasMetrics
import models._
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpClient
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.UpstreamErrorResponse
import utils.Format

import java.time.OffsetDateTime
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageConnector @Inject()(
  config: AppConfig,
  http: HttpClient,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends HasMetrics {

  def post(departureId: DepartureId, message: MessageWithStatus, dateTime: OffsetDateTime, channelType: ChannelType)(
    implicit
    hc: HeaderCarrier): Future[EisSubmissionResult] =
    withMetricsTimerAsync("submit-eis-message") {
      timer =>
        val url = config.eisUrl

        val xmlMessage = TransitWrapper(message.message).toString

        val messageSender = MessageSender(departureId, message.messageCorrelationId)

        val requestHeaders =
          hc.headers(Seq(Constants.XClientIdHeader)) ++ messageHeaders(message.messageType, dateTime, messageSender, channelType)

        http
          .POSTString[Either[UpstreamErrorResponse, Unit]](url, xmlMessage, requestHeaders)
          .map {
            _.fold(
              errorResponse => {
                timer.completeWithFailure()
                responseToStatus(errorResponse)
              },
              _ => {
                timer.completeWithSuccess()
                EisSubmissionSuccessful
              }
            )
          }
    }

  private def messageHeaders(
    messageType: MessageType,
    dateTime: OffsetDateTime,
    messageSender: MessageSender,
    channelType: ChannelType
  ): Seq[(String, String)] =
    Seq(
      HeaderNames.DATE         -> Format.dateFormattedForHeader(dateTime),
      HeaderNames.CONTENT_TYPE -> ContentTypes.XML,
      "X-Message-Type"         -> messageType.toString,
      "X-Message-Sender"       -> messageSender.toString,
      "channel"                -> channelType.toString,
      HeaderNames.ACCEPT       -> "application/xml"
    )
}

object MessageConnector {

  sealed abstract class EisSubmissionResult(val statusCode: Int, val responseBody: String) {
    override def toString: String = s"EisSubmissionResult(code = $statusCode and details = $responseBody)"
  }

  object EisSubmissionResult {
    private val errorResponses = List(ErrorInPayload, VirusFoundOrInvalidToken, DownstreamInternalServerError, DownstreamBadGateway)

    private val responseMapping = errorResponses.map {
      response =>
        response.statusCode -> response
    }.toMap

    def responseToStatus(errorResponse: UpstreamErrorResponse): EisSubmissionResult =
      responseMapping.getOrElse(errorResponse.statusCode, UnexpectedHttpResponse(errorResponse))

    object EisSubmissionSuccessful extends EisSubmissionResult(202, "EIS Successful Submission")

    sealed abstract class EisSubmissionFailure(statusCode: Int, responseBody: String) extends EisSubmissionResult(statusCode, responseBody)

    sealed abstract class EisSubmissionRejected(statusCode: Int, responseBody: String) extends EisSubmissionFailure(statusCode, responseBody)
    object ErrorInPayload                                                              extends EisSubmissionRejected(400, "Message failed schema validation")
    object VirusFoundOrInvalidToken                                                    extends EisSubmissionRejected(403, "Virus found, token invalid etc")

    sealed abstract class EisSubmissionFailureDownstream(statusCode: Int, responseBody: String) extends EisSubmissionFailure(statusCode, responseBody)
    object DownstreamInternalServerError                                                        extends EisSubmissionFailureDownstream(500, "Downstream internal server error")
    object DownstreamBadGateway                                                                 extends EisSubmissionFailureDownstream(502, "Downstream bad gateway ")

    case class UnexpectedHttpResponse(errorResponse: UpstreamErrorResponse)
        extends EisSubmissionFailureDownstream(errorResponse.statusCode, "Unexpected HTTP Response received")
  }
}
