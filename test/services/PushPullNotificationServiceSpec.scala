package services

import base.SpecBase
import config.Constants
import connectors.PushPullNotificationConnector
import models.Box
import models.BoxId
import org.mockito.ArgumentMatchers._
import org.mockito.BDDMockito._
import org.mockito.Mockito.reset
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.test.Helpers._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.UpstreamErrorResponse

import scala.concurrent.ExecutionContext.Implicits.global

class PushPullNotificationServiceSpec extends SpecBase with BeforeAndAfterEach with ScalaCheckPropertyChecks {
  val mockConnector = mock[PushPullNotificationConnector]
  val service       = new PushPullNotificationService(mockConnector)

  override protected def beforeEach(): Unit = reset(mockConnector)

  "PushPullNotificationService" - {

    "getBox" - {
      val testBoxId    = "1c5b9365-18a6-55a5-99c9-83a091ac7f26"
      val testClientId = "X5ZasuQLH0xqKooV_IEw6yjQNfEa"
      val testBox      = Box(BoxId(testBoxId), Constants.BoxName)

      "return a Some(box) when connector call returns 200" in {
        val mockedGetBox     = mockConnector.getBox(anyString())(any[ExecutionContext], any[HeaderCarrier])
        val successfulResult = Future.successful(Right(testBox))

        given(mockedGetBox).willReturn(successfulResult)

        Await.result(service.getBox(testClientId), 30.seconds).mustEqual(Some(testBox))
      }

      "return None when any 4xx or 5xx Http status returned" in {
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

        forAll(errorGenerator) {
          code =>
            val mockedGetBox = mockConnector.getBox(anyString())(any[ExecutionContext], any[HeaderCarrier])
            val testMessage  = "this is a test message"
            val failedResult = Future.successful(Left(UpstreamErrorResponse(testMessage, code)))

            given(mockedGetBox).willReturn(failedResult)

            Await.result(service.getBox(testClientId), 30.seconds).mustEqual(None)
        }
      }
    }
  }
}
