/*
 * Copyright 2020 HM Revenue & Customs
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

import java.time.OffsetDateTime
import com.google.inject.Inject
import config.AppConfig
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult._
import models._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import utils.Format

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MessageConnector @Inject()(config: AppConfig, http: HttpClient)(implicit ec: ExecutionContext) {

  def post(departureId: DepartureId, message: MessageWithStatus, dateTime: OffsetDateTime)(
    implicit headerCarrier: HeaderCarrier
  ): Future[EisSubmissionResult] = {

    val xmlMessage = TransitWrapper(message.message).toString

    val url = config.eisUrl

    lazy val messageSender = MessageSender(departureId, message.messageCorrelationId)

    val newHeaders = headerCarrier
      .copy(authorization = None)
      .withExtraHeaders(addHeaders(message.messageType, dateTime, messageSender): _*)

    // TODO: Don't throw exceptions here
    http
      .POSTString(url, xmlMessage)(rds = HttpReads.readRaw, hc = newHeaders, ec = ec)
      .map(response => responseToStatus(response))
  }

  private def addHeaders(messageType: MessageType, dateTime: OffsetDateTime, messageSender: MessageSender)(
    implicit headerCarrier: HeaderCarrier): Seq[(String, String)] =
    Seq(
      "Date"             -> Format.dateFormattedForHeader(dateTime),
      "Content-Type"     -> "application/xml",
      "Accept"           -> "application/xml",
      "X-Message-Type"   -> messageType.toString,
      "X-Message-Sender" -> messageSender.toString
    )
}

object MessageConnector {

  sealed abstract class EisSubmissionResult(val statusCode: Int, val responseBody: String) {
    override def toString: String = s"EisSubmissionResult(code = $statusCode and details = $responseBody)"
  }

  object EisSubmissionResult {
    private val possibleResponses           = List(EisSubmissionSuccessful, ErrorInPayload, VirusFoundOrInvalidToken, DownstreamInternalServerError)
    private val statusesOfPossibleResponses = possibleResponses.map(_.statusCode)
    private val statusToResponseMapping     = statusesOfPossibleResponses.zip(possibleResponses).toMap

    def responseToStatus(httpResponse: HttpResponse): EisSubmissionResult =
      statusToResponseMapping.getOrElse(httpResponse.status, UnexpectedHttpResponse(httpResponse))
    object EisSubmissionSuccessful extends EisSubmissionResult(202, "EIS Successful Submission")

    sealed abstract class EisSubmissionFailure(statusCode: Int, responseBody: String) extends EisSubmissionResult(statusCode, responseBody)

    sealed abstract class EisSubmissionRejected(statusCode: Int, responseBody: String) extends EisSubmissionFailure(statusCode, responseBody)
    object ErrorInPayload                                                              extends EisSubmissionRejected(400, "Message failed schema validation")
    object VirusFoundOrInvalidToken                                                    extends EisSubmissionRejected(403, "Virus found, token invalid etc")

    sealed abstract class EisSubmissionFailureDownstream(statusCode: Int, responseBody: String) extends EisSubmissionFailure(statusCode, responseBody)
    object DownstreamInternalServerError                                                        extends EisSubmissionFailureDownstream(500, "Downstream internal server error")
    case class UnexpectedHttpResponse(httpResponse: HttpResponse)
        extends EisSubmissionFailureDownstream(httpResponse.status, "Unexpected HTTP Response received")
  }
}
