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

package testOnly.services

import base.SpecBase
import cats.data.NonEmptyList
import models.DepartureId
import models.Message
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import testOnly.models.SeedDataParameters
import testOnly.models.SeedEori

import java.time.Clock
import java.time.Instant
import java.time.ZoneId

class TestOnlySeedDataServiceSpec extends SpecBase with ScalaCheckDrivenPropertyChecks {
  implicit def dontShrink[A]: Shrink[A] = Shrink.shrinkAny

  val testClock               = Clock.fixed(Instant.now(), ZoneId.systemDefault)
  val testDataGenerator       = new TestDataGenerator(testClock)
  val testOnlySeedDataService = new TestOnlySeedDataService(testDataGenerator)

  "seedDepartures" - {

    "returns an iterator of departures with DepartureIds for the number of users and movements specified" in {

      val seedEori  = SeedEori("ZZ", 1, 12)
      val seedEori1 = SeedEori("ZZ", 2, 12)

      val seedDataParameters = SeedDataParameters(2, 2, DepartureId(0), None, departureOffice = None, None)

      val expectedResult = Seq(
        (DepartureId(0), seedEori.format),
        (DepartureId(1), seedEori.format),
        (DepartureId(2), seedEori1.format),
        (DepartureId(3), seedEori1.format)
      )

      val result =
        testOnlySeedDataService
          .seedDepartures(seedDataParameters)
          .toSeq
          .map(x => (x.departureId, x.eoriNumber))

      result mustBe expectedResult
    }

    "returns an iterator of departures with dynamic xml" in {

      val seedDataParameters = SeedDataParameters(2, 2, DepartureId(0), None, departureOffice = None, None)

      val expectedEori1 = SeedEori("ZZ", 1, 12)
      val expectedEori2 = SeedEori("ZZ", 2, 12)

      val result: Seq[NonEmptyList[Message]] =
        testOnlySeedDataService
          .seedDepartures(seedDataParameters)
          .toSeq
          .map(_.messages)

      result.length mustBe 4

      (result.head.head.message \\ "TRAPRIPC1" \\ "TINPC159").text mustBe expectedEori1.format
      (result(1).head.message \\ "TRAPRIPC1" \\ "TINPC159").text mustBe expectedEori1.format
      (result(2).head.message \\ "TRAPRIPC1" \\ "TINPC159").text mustBe expectedEori2.format
      (result(3).head.message \\ "TRAPRIPC1" \\ "TINPC159").text mustBe expectedEori2.format
    }

    "the total number of Departures from the iterator is the (Number users x number of movements)" in {
      val genInt = Gen.choose(10, 50)

      forAll(genInt, genInt) {
        (eoriCount, mrnCount) =>
          val seedDataParameters = SeedDataParameters(eoriCount, mrnCount, DepartureId(0), None, departureOffice = None, None)

          val iterator = testOnlySeedDataService.seedDepartures(seedDataParameters)

          iterator.length mustEqual (eoriCount * mrnCount)
      }
    }
  }

}
