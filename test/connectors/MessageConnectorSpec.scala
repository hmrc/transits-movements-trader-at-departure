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

import java.time.LocalDateTime
import java.time.OffsetDateTime

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.matching.StringValuePattern
import config.AppConfig
import generators.ModelGenerators
import models.DepartureId
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
import play.api.Configuration
import play.api.Environment
import play.api.Mode
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.RunMode
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

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

  private def connector: MessageConnector = app.injector.instanceOf[MessageConnector]

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val messageType: MessageType = Gen.oneOf(MessageType.values).sample.value

  private val env           = Environment.simple()
  private val configuration = Configuration.load(env)

  private val serviceConfig = new ServicesConfig(configuration, new RunMode(configuration, Mode.Dev))
  private val appConfig     = new AppConfig(configuration, serviceConfig)

  "MessageConnector" - {

    "post" - {

      "return HttpResponse with status Accepted when when post is successful with Accepted" in {

        val messageSender = "MDTP-000000000000000000000000123-01"

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader("X-Forwarded-Host", equalTo("mdtp"))
            .withHeader("X-Correlation-ID", matching("""\b[0-9a-f]{8}\b-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-\b[0-9a-f]{12}\b"""))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/xml"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .withRequestBody(matchingXPath("/transitRequest"))
            .willReturn(
              aResponse()
                .withStatus(202)
            )
        )

        val postValue   = MessageWithStatus(LocalDateTime.now(), messageType, <CC007A>test</CC007A>, MessageStatus.SubmissionPending, 1)
        val departureId = DepartureId(123)

        val result = connector.post(departureId, postValue, OffsetDateTime.now())

        whenReady(result) {
          response =>
            response.status mustBe 202
        }
      }

      "return an exception when post is unsuccessful" in {

        val messageSender = "MDTP-000000000000000000000000123-01"

        server.stubFor(
          post(urlEqualTo(postUrl))
            .withHeader("X-Forwarded-Host", equalTo("mdtp"))
            .withHeader("X-Correlation-ID", matching("""\b[0-9a-f]{8}\b-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-\b[0-9a-f]{12}\b"""))
            .withHeader("Content-Type", equalTo("application/xml"))
            .withHeader("Accept", equalTo("application/xml"))
            .withHeader("X-Message-Type", equalTo(messageType.toString))
            .withHeader("X-Message-Sender", equalTo(messageSender))
            .willReturn(
              aResponse()
                .withStatus(genFailedStatusCodes.sample.value)
            )
        )

        val postValue   = MessageWithStatus(LocalDateTime.now(), messageType, <CC007A>test</CC007A>, MessageStatus.SubmissionPending, 1)
        val departureId = DepartureId(123)

        val result = connector.post(departureId, postValue, OffsetDateTime.now())

        whenReady(result.failed) {
          response =>
            response mustBe an[Exception]
        }

      }
    }
  }
}

object MessageConnectorSpec {

  private val postUrl                        = "/movements/messages"
  private val genFailedStatusCodes: Gen[Int] = Gen.choose(400, 599)
}
