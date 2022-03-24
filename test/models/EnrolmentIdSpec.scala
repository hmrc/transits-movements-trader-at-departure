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

package models

import base.SpecBase
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers

class EnrolmentIdSpec extends SpecBase with Matchers {

  // Ior[TURN, EORINumber]

  "when EnrolmentId contains only a TURN" - {

    val enrolmentId: EnrolmentID = EnrolmentID(Option(TURN("ABC")), None)

    "check that we can return a TURN" in {

      enrolmentId.turn mustEqual Some(TURN("ABC"))

    }

    "check that we don't have an EORI" in {

      enrolmentId.eoriNumber mustEqual None

    }

    "customerId returns the TURN value" in Seq("ABC", "DEF").foreach {
      arbitaryTurn =>
        val enrolmentId: EnrolmentID = EnrolmentID(Option(TURN(arbitaryTurn)), None)
        enrolmentId.customerId mustEqual arbitaryTurn
    }

  }

  "when EnrolmentId contains only a EORINumber" - {

    val enrolmentId: EnrolmentID = EnrolmentID(None, Option(EORINumber("ABC")))

    "check that we can return a EORINumber" in {

      enrolmentId.eoriNumber mustEqual Some(EORINumber("ABC"))

    }

    "check that we don't have an TURN" in {
      enrolmentId.turn mustEqual None
    }

    "customerId returns the EORINumber value" in Seq("ABC", "DEF").foreach {
      arbitaryTurn =>
        val enrolmentId: EnrolmentID = EnrolmentID(None, Option(EORINumber(arbitaryTurn)))
        enrolmentId.customerId mustEqual arbitaryTurn
    }
  }

  "when EnrolmentId contains a EORINumber and a TURN return the EORINumber" - {

    val enrolmentId: EnrolmentID = EnrolmentID(Option(TURN("ABC")), Option(EORINumber("DEF")))

    "check that we can return a EORINumber" in {

      enrolmentId.customerId mustEqual "DEF"
    }

  }

}
