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

import com.github.tomakehurst.wiremock.client.WireMock._
import connectors.MessageConnector.EisSubmissionResult.DownstreamBadGateway
import connectors.MessageConnector.EisSubmissionResult.DownstreamInternalServerError
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
import generators.ModelGenerators
import models.ChannelType.Api
import models.ChannelType.Web
import models.DepartureId
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MessageWithStatus
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

import java.time.LocalDateTime
import java.time.OffsetDateTime

class MessageConnectorSpec
    extends AnyFreeSpec
    with MockitoSugar
    with ScalaFutures
    with Matchers
    with IntegrationPatience
    with WiremockSuite
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  import MessageConnectorSpec._

  override protected def portConfigKey: String = "microservice.services.eis.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier().copy(requestId = Some(RequestId("bar")), otherHeaders = Seq("X-Client-Id" -> "foo"))

  private val messageType: MessageType = Gen.oneOf(MessageType.values).sample.value

  private val channelGen = Gen.oneOf(Web, Api)

  "MessageConnector" - {

    "post" - {

      "return HttpResponse with status Accepted when when post is successful with Accepted" in {

        forAll(channelGen) {
          channel =>
            val messageSender = "MDTP-DEP-00000000000000000000123-01"

            server.stubFor(
              post(urlEqualTo(postUrl))
                .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
                .withHeader("X-Client-Id", equalTo("foo"))
                .withHeader("X-Request-Id", equalTo("bar"))
                .withHeader("X-Message-Type", equalTo(messageType.toString))
                .withHeader("X-Message-Sender", equalTo(messageSender))
                .withHeader("channel", equalTo(channel.toString))
                .withHeader(HeaderNames.ACCEPT, equalTo(acceptHeader))
                .withRequestBody(matchingXPath("/transitRequest"))
                .willReturn(
                  aResponse()
                    .withStatus(202)
                )
            )
            val app = appBuilder.build()
            val postValue = MessageWithStatus(
              MessageId(1),
              LocalDateTime.now(),
              messageType,
              <CC007A>test</CC007A>,
              MessageStatus.SubmissionPending,
              1,
              convertXmlToJson(<CC007A>test</CC007A>.toString)
            )
            val departureId = DepartureId(123)

            running(app) {
              val connector = app.injector.instanceOf[MessageConnector]
              val result    = connector.post(departureId, postValue, OffsetDateTime.now(), channel)
              result.futureValue mustEqual EisSubmissionSuccessful
            }
        }
      }

      "return a BAD_GATEWAY for a return code of 502" in {

        forAll(channelGen) {
          channel =>
            val messageSender = "MDTP-DEP-00000000000000000000123-01"

            server.stubFor(
              post(urlEqualTo(postUrl))
                .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
                .withHeader("X-Client-Id", equalTo("foo"))
                .withHeader("X-Request-Id", equalTo("bar"))
                .withHeader("X-Message-Type", equalTo(messageType.toString))
                .withHeader("X-Message-Sender", equalTo(messageSender))
                .withHeader(HeaderNames.ACCEPT, equalTo(acceptHeader))
                .withHeader("channel", equalTo(channel.toString))
                .willReturn(
                  aResponse()
                    .withStatus(502)
                )
            )

            val postValue = MessageWithStatus(
              MessageId(1),
              LocalDateTime.now(),
              messageType,
              <CC007A>test</CC007A>,
              MessageStatus.SubmissionPending,
              1,
              convertXmlToJson(<CC007A>test</CC007A>.toString)
            )
            val departureId = DepartureId(123)
            val app         = appBuilder.build()

            running(app) {
              val connector = app.injector.instanceOf[MessageConnector]
              val result    = connector.post(departureId, postValue, OffsetDateTime.now(), channel)
              result.futureValue mustEqual DownstreamBadGateway
            }
        }

      }

      "return a BAD_GATEWAY for a return code of 500" in {

        forAll(channelGen) {
          channel =>
            val messageSender = "MDTP-DEP-00000000000000000000123-01"

            server.stubFor(
              post(urlEqualTo(postUrl))
                .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
                .withHeader("X-Client-Id", equalTo("foo"))
                .withHeader("X-Request-Id", equalTo("bar"))
                .withHeader("X-Message-Type", equalTo(messageType.toString))
                .withHeader("X-Message-Sender", equalTo(messageSender))
                .withHeader(HeaderNames.ACCEPT, equalTo(acceptHeader))
                .withHeader("channel", equalTo(channel.toString))
                .willReturn(
                  aResponse()
                    .withStatus(500)
                )
            )

            val postValue = MessageWithStatus(
              MessageId(1),
              LocalDateTime.now(),
              messageType,
              <CC007A>test</CC007A>,
              MessageStatus.SubmissionPending,
              1,
              convertXmlToJson(<CC007A>test</CC007A>.toString())
            )
            val departureId = DepartureId(123)
            val app         = appBuilder.build()

            running(app) {
              val connector = app.injector.instanceOf[MessageConnector]
              val result    = connector.post(departureId, postValue, OffsetDateTime.now(), channel)
              result.futureValue mustEqual DownstreamInternalServerError
            }

        }

      }

      "return an UnexpectedHttpResonse for an error code other than 202, 400, 403, 500 and 502" in {

        forAll(channelGen) {
          channel =>
            val messageSender = "MDTP-DEP-00000000000000000000123-01"

            server.stubFor(
              post(urlEqualTo(postUrl))
                .withHeader(HeaderNames.CONTENT_TYPE, equalTo(ContentTypes.XML))
                .withHeader("X-Client-Id", equalTo("foo"))
                .withHeader("X-Request-Id", equalTo("bar"))
                .withHeader("X-Message-Type", equalTo(messageType.toString))
                .withHeader("X-Message-Sender", equalTo(messageSender))
                .withHeader(HeaderNames.ACCEPT, equalTo(acceptHeader))
                .withHeader("channel", equalTo(channel.toString))
                .willReturn(
                  aResponse()
                    .withStatus(418)
                )
            )

            val postValue = MessageWithStatus(
              MessageId(1),
              LocalDateTime.now(),
              messageType,
              <CC007A>test</CC007A>,
              MessageStatus.SubmissionPending,
              1,
              convertXmlToJson(<CC007A>test</CC007A>.toString())
            )
            val departureId = DepartureId(123)
            val app         = appBuilder.build()

            running(app) {
              val connector = app.injector.instanceOf[MessageConnector]
              val result    = connector.post(departureId, postValue, OffsetDateTime.now(), channel)
              result.futureValue.statusCode mustEqual 418
            }
        }
      }

      "EisSubmissionResult toString must equal expected result" in {
        val status = MessageConnector.EisSubmissionResult.EisSubmissionSuccessful
        status.toString mustBe "EisSubmissionResult(code = 202 and details = EIS Successful Submission)"
      }
    }
  }
}

object MessageConnectorSpec {

  private val postUrl                        = "/movements/messages"
  private val genFailedStatusCodes: Gen[Int] = Gen.choose(400, 599)
  private val acceptHeader                   = "application/xml"
}
