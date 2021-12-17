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

package models.response

import models.DepartureWithoutMessages
import play.api.libs.json.Json
import play.api.libs.json.OWrites

case class ResponseDepartureSummaries(departureSummaries: Seq[ResponseDepartureSummary], retrievedDepartures: Int, totalDepartures: Int, totalMatched: Int)

object ResponseDepartureSummaries {
  implicit val writes: OWrites[ResponseDepartureSummaries] = Json.writes[ResponseDepartureSummaries]

  def build(results: Seq[DepartureWithoutMessages], totalDepartures: Int, totalMatched: Int): ResponseDepartureSummaries =
    ResponseDepartureSummaries(
      results.map(ResponseDepartureSummary.build),
      results.length,
      totalDepartures = totalDepartures,
      totalMatched = totalMatched
    )
}
