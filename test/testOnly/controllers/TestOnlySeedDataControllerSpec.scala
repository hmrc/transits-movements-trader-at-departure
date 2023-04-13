/*
 * Copyright 2023 HM Revenue & Customs
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

package testOnly.controllers

import base._
import generators.ModelGenerators
import models.ChannelType
import models.DepartureId
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Application
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsJson
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.DepartureRepository

import java.time.Clock
import java.time.Instant
import java.time.ZoneId
import scala.concurrent.Future
import repositories.DepartureIdRepository

class TestOnlySeedDataControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with GuiceOneAppPerSuite with BeforeAndAfterEach {

  val mockRepository   = mock[DepartureRepository]
  val mockIdRepository = mock[DepartureIdRepository]

  override def beforeEach: Unit =
    Mockito.reset(mockRepository, mockIdRepository)

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure("play.http.router" -> "testOnlyDoNotUseInAppConf.Routes", "feature-flags.testOnly.enabled" -> true)
      .overrides(
        bind[Clock].toInstance(Clock.fixed(Instant.now(), ZoneId.systemDefault)),
        bind[DepartureRepository].toInstance(mockRepository),
        bind[DepartureIdRepository].toInstance(mockIdRepository)
      )
      .build()

  "seedData" - {

    "must return OK with details of the seeded data" - {

      "when there no first Eori value specified" in {
        when(mockRepository.bulkInsert(any())).thenReturn(Future.successful(()))
        when(mockRepository.getMaxDepartureId).thenReturn(Future.successful(Some(DepartureId(134))))
        when(mockIdRepository.setLatestId(any())).thenReturn(Future.successful(()))

        val request: FakeRequest[AnyContentAsJson] = FakeRequest(POST, testOnly.controllers.routes.TestOnlySeedDataController.seedData.url)
          .withHeaders("channel" -> ChannelType.Web.toString)
          .withJsonBody(
            Json
              .parse("""
                                     |{
                                     |  "numberOfUsers": 100,
                                     |  "movementsPerUser": 10,
                                     |  "startDepartureId": 10
                                     |}""".stripMargin)
              .as[JsObject]
          )

        val result = route(app, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.obj(
          "eoriRangeStart"         -> "ZZ000000000001",
          "eoriRangeEnd"           -> "ZZ000000000101",
          "numberOfUsers"          -> 100,
          "movementsPerUser"       -> 10,
          "totalInsertedMovements" -> 1000,
          "departureIdRangeStart"  -> 10,
          "departureIdRangeEnd"    -> 1009
        )

        verify(mockRepository, times(20)).bulkInsert(any())
        verify(mockIdRepository, times(1)).setLatestId(eqTo(134))
      }

      "when there is a first Eori value specified" in {
        when(mockRepository.bulkInsert(any())).thenReturn(Future.successful(()))
        when(mockRepository.getMaxDepartureId).thenReturn(Future.successful(Some(DepartureId(312))))
        when(mockIdRepository.setLatestId(any())).thenReturn(Future.successful(()))

        val request: FakeRequest[AnyContentAsJson] = FakeRequest(POST, testOnly.controllers.routes.TestOnlySeedDataController.seedData.url)
          .withHeaders("channel" -> ChannelType.Web.toString)
          .withJsonBody(
            Json
              .parse("""
                                     |{
                                     |  "startEori": "ZZ000000000021",
                                     |  "numberOfUsers": 100,
                                     |  "movementsPerUser": 10,
                                     |  "startDepartureId": 10
                                     |}""".stripMargin)
              .as[JsObject]
          )

        val result = route(app, request).value

        status(result) mustEqual OK
        contentAsJson(result) mustEqual Json.obj(
          "eoriRangeStart"         -> "ZZ000000000021",
          "eoriRangeEnd"           -> "ZZ000000000121",
          "numberOfUsers"          -> 100,
          "movementsPerUser"       -> 10,
          "totalInsertedMovements" -> 1000,
          "departureIdRangeStart"  -> 10,
          "departureIdRangeEnd"    -> 1009
        )

        verify(mockRepository, times(20)).bulkInsert(any())
        verify(mockIdRepository, times(1)).setLatestId(eqTo(312))
      }

    }
  }
}
