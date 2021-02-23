package api.helpers

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, equalTo, post, urlEqualTo, any}
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
