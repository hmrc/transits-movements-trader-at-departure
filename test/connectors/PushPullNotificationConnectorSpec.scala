package connectors

import org.scalatest.freespec.AnyFreeSpec
import uk.gov.hmrc.http.HeaderCarrier
import com.github.tomakehurst.wiremock.client.WireMock._
import play.api.test.Helpers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.matchers.must.Matchers
import config.Constants
import models.Box
import models.BoxId

import scala.concurrent.ExecutionContext.Implicits.global

class PushPullNotificationConnectorSpec extends AnyFreeSpec with WiremockSuite with ScalaFutures with Matchers with IntegrationPatience {
  override protected def portConfigKey: String = "microservice.services.push-pull-notifications-api.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

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
  }

}
