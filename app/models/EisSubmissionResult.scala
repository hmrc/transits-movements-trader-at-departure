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

package models

import uk.gov.hmrc.http.HttpResponse

sealed abstract class EisSubmissionResult(val httpStatus: Int, val asString: String) {
  override def toString: String = s"EisSubmissionResult(code = $httpStatus, and details = " + asString + ")"
}

object EisSubmissionResult {
  private val possibleResponses           = List(EisSubmissionSuccessful, ErrorInPayload, VirusFoundOrInvalidToken, DownstreamInternalServerError)
  private val statusesOfPossibleResponses = possibleResponses.map(_.httpStatus)
  private val statusToResponseMapping     = statusesOfPossibleResponses.zip(possibleResponses).toMap

  def responseToStatus(httpResponse: HttpResponse): EisSubmissionResult =
    statusToResponseMapping.getOrElse(httpResponse.status, UnexpectedHttpResponse(httpResponse))
  object EisSubmissionSuccessful extends EisSubmissionResult(202, "EIS Successful Submission")

  sealed abstract class EisSubmissionFailure(httpStatus: Int, asString: String) extends EisSubmissionResult(httpStatus, asString)

  sealed abstract class EisSubmissionRejected(httpStatus: Int, asString: String) extends EisSubmissionFailure(httpStatus, asString)
  object ErrorInPayload                                                          extends EisSubmissionRejected(400, "Message failed schema validation")
  object VirusFoundOrInvalidToken                                                extends EisSubmissionRejected(403, "Virus found, token invalid etc")

  sealed abstract class EisSubmissionFailureDownstream(httpStatus: Int, asString: String) extends EisSubmissionFailure(httpStatus, asString)
  object DownstreamInternalServerError                                                    extends EisSubmissionFailureDownstream(500, "Downstream internal server error")
  case class UnexpectedHttpResponse(httpResponse: HttpResponse)                           extends EisSubmissionFailureDownstream(httpResponse.status, "Unexpected HTTP Response received")
}
