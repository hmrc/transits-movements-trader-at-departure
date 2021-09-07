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

import akka.util.ByteString
import com.github.tomakehurst.wiremock.client.WireMock._
import generators.ModelGenerators
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import play.api.test.Helpers.running
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.RequestId

import scala.concurrent.Future
import scala.xml.Elem

class ManageDocumentsConnectorSpec
    extends AnyFreeSpec
    with MockitoSugar
    with ScalaFutures
    with Matchers
    with IntegrationPatience
    with WiremockSuite
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  override protected def portConfigKey: String = "microservice.services.manage-documents.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier().copy(requestId = Some(RequestId("bar")), otherHeaders = Seq("X-Client-Id" -> "foo"))

  val releasedForTransitXml: Elem = <releaseForTransit>
    <fieldTwo>Field Twos Value</fieldTwo>
  </releaseForTransit>

  val expectedXml: String =
    """<releaseForTransit>
      |   <fieldTwo>Field Twos Value</fieldTwo>
      |  </releaseForTransit>
      |""".stripMargin

  "ManageDocumentsConnector" - {

    "getTadPDF" - {

      "must return status Ok and PDF" in {

        server.stubFor(
          post(urlEqualTo("/transit-movements-trader-manage-documents/transit-accompanying-document"))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader(HeaderNames.USER_AGENT, equalTo("transits-movements-trader-at-departure"))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withRequestBody(equalToXml(expectedXml))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody("Hello")
            )
        )

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[ManageDocumentsConnector]

          val result: Future[Either[TADErrorResponse, (ByteString, Map[String, Seq[String]])]] = connector.getTadPDF(releasedForTransitXml)
          result.futureValue.isRight mustBe true
        }
      }

      "must return other response without exceptions" in {

        val genErrorResponse = Gen.oneOf(300, 500).sample.value

        server.stubFor(
          post(urlEqualTo("/transit-movements-trader-manage-documents/transit-accompanying-document"))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader(HeaderNames.USER_AGENT, equalTo("transits-movements-trader-at-departure"))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withRequestBody(equalToXml(expectedXml))
            .willReturn(
              aResponse()
                .withStatus(genErrorResponse)
            )
        )

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[ManageDocumentsConnector]

          val result: Future[Either[TADErrorResponse, (ByteString, Map[String, Seq[String]])]] = connector.getTadPDF(releasedForTransitXml)
          result.futureValue mustBe Left(UnexpectedResponse(genErrorResponse))
        }
      }
    }

    "getTsadPDF" - {

      "must return status Ok and PDF" in {

        server.stubFor(
          post(urlEqualTo("/transit-movements-trader-manage-documents/transit-security-accompanying-document"))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader(HeaderNames.USER_AGENT, equalTo("transits-movements-trader-at-departure"))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withRequestBody(equalToXml(expectedXml))
            .willReturn(
              aResponse()
                .withStatus(200)
                .withBody("Hello")
            )
        )

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[ManageDocumentsConnector]

          val result: Future[Either[TADErrorResponse, (ByteString, Map[String, Seq[String]])]] = connector.getTsadPDF(releasedForTransitXml)
          result.futureValue.isRight mustBe true
        }
      }

      "must return other response without exceptions" in {

        val genErrorResponse = Gen.oneOf(300, 500).sample.value

        server.stubFor(
          post(urlEqualTo("/transit-movements-trader-manage-documents/transit-security-accompanying-document"))
            .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
            .withHeader(HeaderNames.USER_AGENT, equalTo("transits-movements-trader-at-departure"))
            .withHeader("X-Client-Id", equalTo("foo"))
            .withHeader("X-Request-Id", equalTo("bar"))
            .withRequestBody(equalToXml(expectedXml))
            .willReturn(
              aResponse()
                .withStatus(genErrorResponse)
            )
        )

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[ManageDocumentsConnector]

          val result: Future[Either[TADErrorResponse, (ByteString, Map[String, Seq[String]])]] = connector.getTsadPDF(releasedForTransitXml)
          result.futureValue mustBe Left(UnexpectedResponse(genErrorResponse))
        }
      }
    }
  }
}
