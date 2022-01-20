/*
 * Copyright 2022 HM Revenue & Customs
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
import config.Constants
import models.Box
import models.BoxId
import models.DepartureId
import models.DepartureMessageNotification
import models.MessageId
import models.MessageType
import org.scalacheck.Gen
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import play.api.test.Helpers._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import java.time.LocalDateTime

class PushPullNotificationConnectorSpec extends AnyFreeSpec with WiremockSuite with ScalaFutures with Matchers with IntegrationPatience {
  override protected def portConfigKey: String = "microservice.services.push-pull-notifications-api.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private def requestId(departureId: DepartureId): String =
    s"/customs/transits/movements/departures/${departureId.index}"

  "PushPullNotificationConnector" - {

    "getBox" - {

      val testBoxId    = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
      val testClientId = "X5ZasuQLH0xqKooV_IEw6yjQNfEa"

      "should return a Right[Box] when the pushPullNotification API returns 200 and valid JSON" in {
        server.stubFor {
          get(urlPathEqualTo("/box")).willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(s"""
                {
                  "boxId": "$testBoxId",
                  "boxName":"${Constants.BoxName}",
                  "boxCreator":{
                      "clientId": "$testClientId"
                  },
                  "subscriber": {
                      "subscribedDateTime": "2020-06-01T10:27:33.613+0000",
                      "callBackUrl": "https://www.example.com/callback",
                      "subscriptionType": "API_PUSH_SUBSCRIBER"
                  }
                }
              """)
          )
        }

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[PushPullNotificationConnector]
          val result    = connector.getBox(testClientId)

          result.futureValue.right.get mustEqual Box(BoxId(testBoxId), Constants.BoxName)
        }

      }

      "should return Left when the pushPullNotification API returns 404" in {
        server.stubFor {
          get(urlPathEqualTo("/box")).willReturn(
            aResponse()
              .withStatus(NOT_FOUND)
          )
        }

        val app = appBuilder.build()

        running(app) {
          val connector    = app.injector.instanceOf[PushPullNotificationConnector]
          val futureResult = connector.getBox(testClientId)
          val result       = futureResult.futureValue

          assert(result.isLeft)
          result.left.get.statusCode mustBe NOT_FOUND
        }
      }

      "should return Left when the pushPullNotification API returns 500" in {
        server.stubFor {
          get(urlPathEqualTo("/box")).willReturn(
            aResponse()
              .withStatus(INTERNAL_SERVER_ERROR)
          )
        }

        val app = appBuilder.build()

        running(app) {
          val connector    = app.injector.instanceOf[PushPullNotificationConnector]
          val futureResult = connector.getBox(testClientId)
          val result       = futureResult.futureValue

          assert(result.isLeft)
          result.left.get.statusCode mustBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "postNotification" - {

      val testBoxId       = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
      val testUrlPath     = s"/box/$testBoxId/notifications"
      val testBody        = <test>test content</test>
      val testEori        = "1234567800"
      val testDepartureId = DepartureId(1)
      val testMessageUri  = requestId(testDepartureId) + "/messages" + ""
      val testNotification =
        DepartureMessageNotification(
          testMessageUri,
          requestId(testDepartureId),
          testEori,
          testDepartureId,
          MessageId(2),
          LocalDateTime.now,
          MessageType.DepartureDeclaration,
          Some(testBody)
        )

      "should return a Right[Unit] when the notification is successfully POSTed" in {

        server.stubFor {
          post(urlPathEqualTo(testUrlPath)).willReturn(aResponse().withStatus(CREATED))
        }

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[PushPullNotificationConnector]
          val result    = connector.postNotification(BoxId(testBoxId), testNotification)

          result.futureValue.right.get.mustEqual(())
        }
      }

      "should return a Left[UpstreamErrorResponse] when any 4xx or 5xx error is returned" in {
        val errorGenerator: Gen[Int] = Gen.oneOf(
          Seq(
            INTERNAL_SERVER_ERROR,
            BAD_REQUEST,
            FORBIDDEN,
            GATEWAY_TIMEOUT,
            NOT_FOUND,
            NOT_IMPLEMENTED,
            SERVICE_UNAVAILABLE,
            UNAUTHORIZED
          )
        )

        val app = appBuilder.build()

        running(app) {
          forAll(errorGenerator) {
            code =>
              server.stubFor {
                post(urlPathEqualTo(testUrlPath)).willReturn(aResponse().withStatus(code))
              }

              val connector = app.injector.instanceOf[PushPullNotificationConnector]
              val result    = connector.postNotification(BoxId(testBoxId), testNotification)

              result.futureValue.left.get.statusCode mustBe code

          }
        }
      }

    }
  }

}
