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

package repositories

import models.DepartureId
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.ExecutionContext.Implicits.global

class TestOnlyDepartureIdRepositorySpec extends AnyFreeSpec with Matchers with MongoSuite with GuiceOneAppPerSuite {

  override lazy val fakeApplication: Application =
    new GuiceApplicationBuilder()
      .configure(
        "play.http.router"               -> "testOnlyDoNotUseInAppConf.Routes",
        "feature-flags.testOnly.enabled" -> true
      )
      .build()

  private val service = app.injector.instanceOf[DepartureIdRepository]

  "DepartureIdRepository" - {
    "must allow setting next DepartureId when testOnly features are enabled" in {
      database.flatMap(_.drop()).futureValue
      service.setLatestId(3).futureValue
      service.nextId().futureValue mustBe DepartureId(4)
    }
  }
}
