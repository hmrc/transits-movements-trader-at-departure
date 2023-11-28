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
import models.DepartureIdWrapper
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.Application
import play.api.Configuration
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.ExecutionContext.Implicits.global

class DepartureIdRepositorySpec extends AnyFreeSpec with Matchers with GuiceOneAppPerSuite with DefaultPlayMongoRepositorySupport[DepartureIdWrapper] {

  override lazy val fakeApplication: Application =
    new GuiceApplicationBuilder()
      .configure(
        "play.http.router"               -> "testOnlyDoNotUseInAppConf.Routes",
        "feature-flags.testOnly.enabled" -> false
      )
      .build()

  private val config: Configuration = fakeApplication.injector.instanceOf[Configuration]
  override lazy val repository      = new DepartureIdRepositoryImpl(mongoComponent, config)

  "DepartureIdRepository" - {

    "must generate sequential DepartureIds starting at 1 when no record exists within the database" in {

      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe DepartureId(1)
      second mustBe DepartureId(2)
    }

    "must generate sequential DepartureIds when a record exists within the database" in {

      repository.collection.insertOne(DepartureIdWrapper(1)).toFuture().futureValue

      val first  = repository.nextId().futureValue
      val second = repository.nextId().futureValue

      first mustBe DepartureId(2)
      second mustBe DepartureId(3)
    }

    "must not allow setting next DepartureId when testOnly features are disabled" in {
      intercept[Exception] {
        repository.setLatestId(3).futureValue
      }
    }
  }

}
