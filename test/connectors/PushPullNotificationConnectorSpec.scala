package connectors

import org.scalatest.freespec.AnyFreeSpec
import uk.gov.hmrc.http.HeaderCarrier
import com.github.tomakehurst.wiremock.client.WireMock._
import play.api.test.Helpers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.matchers.must.Matchers
import models.Box
import models.BoxId

import scala.concurrent.ExecutionContext.Implicits.global

class PushPullNotificationConnectorSpec extends AnyFreeSpec with WiremockSuite with ScalaFutures with Matchers with IntegrationPatience {
  override protected def portConfigKey: String = "microservice.services.push-pull-notifications-api.port"

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "PushPullNotificationConnector" - {

    "getBox" - {

      val testBoxId    = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
      val testBoxName  = "BOX 2"
      val testClientId = "X5ZasuQLH0xqKooV_IEw6yjQNfEa"

      "should return a Some(Box) when the pushPullNotification API returns 200 and valid JSON" in {
        server.stubFor {
          get(urlPathEqualTo("/box")).willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(s"""
                {
                  "boxId": "$testBoxId",
                  "boxName":"$testBoxName",
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
          val result    = connector.getBox(testBoxName, testClientId)

          result.futureValue mustEqual Some(Box(BoxId(testBoxId), testBoxName))
        }

      }

      "should return None when the pushPullNotification API returns 404" in {
        server.stubFor {
          get(urlPathEqualTo("/box")).willReturn(
            aResponse()
              .withStatus(NOT_FOUND)
          )
        }

        val app = appBuilder.build()

        running(app) {
          val connector = app.injector.instanceOf[PushPullNotificationConnector]
          val result    = connector.getBox(testBoxName, testClientId)

          result.futureValue mustEqual None
        }
      }
    }
  }

}
