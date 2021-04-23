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

package api.helpers

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, equalTo, post, urlEqualTo}
import com.github.tomakehurst.wiremock.stubbing.StubMapping

trait StubHelper {

  protected val server: WireMockServer

  def stubForPostWithResponseBody(url: String, body: Array[Byte], requestId: String, extraHeaders: Seq[(String, String)] = Nil, status: Int = 200): StubMapping = {
    server.stubFor(
      extraHeaders.foldLeft(
        post(urlEqualTo(url))
          .withHeader("User-Agent", equalTo("transits-movements-trader-at-departure"))
          .withHeader("X-Request-Id", equalTo(requestId))
      ) {
        case (builder, (key, value)) => builder.withHeader(key, equalTo(value))
      }.willReturn(
        aResponse()
          .withStatus(status)
          .withBody(body)
      )
    )
  }
}
