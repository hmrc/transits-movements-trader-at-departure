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

import com.mongodb.client.model.Filters
import config.AppConfig
import generators.ModelGenerators
import models.DepartureId
import models.DepartureLock
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec
    extends AnyFreeSpec
    with Matchers
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with GuiceOneAppPerSuite
    with DefaultPlayMongoRepositorySupport[DepartureLock] {
  private val appConfig: AppConfig = app.injector.instanceOf[AppConfig]
  override lazy val repository     = new LockRepositoryImpl(mongoComponent, appConfig)
  override def afterAll(): Unit    = dropDatabase()

  "lock" - {
    "must lock an departureId when it is not already locked" in {
      val departureId = DepartureId(1)
      val result      = repository.lock(departureId).futureValue
      result mustEqual true
      val selector = Filters.eq("_id", departureId)

      val lock = repository.collection.find(selector).head().futureValue

      lock.id mustEqual departureId.index.toString
    }

    "must not lock an departureId that is already locked" in {

      val departureId = DepartureId(1)

      val result1 = repository.lock(departureId).futureValue
      val result2 = repository.lock(departureId).futureValue

      result1 mustEqual true
      result2 mustEqual false
    }

  }

  "unlock" - {
    "must remove an existing lock" in {

      val departureId = DepartureId(1)
      repository.lock(departureId).futureValue
      repository.unlock(departureId).futureValue

      val selector      = Filters.eq("_id", departureId)
      val remainingLock = repository.collection.find(selector).head().futureValue

      // remainingLock must not be defined[DepartureLock]

    }
    "must not fail when asked to remove a lock that doesn't exist" in {
      val departureId = DepartureId(1)
      repository.unlock(departureId).futureValue
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" in {
      val departureId = DepartureId(1)
      val result      = repository.lock(departureId).futureValue

      result mustEqual true

      val selector = Filters.and(Filters.eq("_id", departureId), Filters.`type`("created", "date"))

      //repository.collection.find(selector).head().futureValue must be(defined[DepartureLock])

    }

  }

}
