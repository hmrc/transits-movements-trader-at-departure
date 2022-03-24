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
import cats.data.Ior
import org.scalatest.matchers.must.Matchers

class EnrolmentIdSpec extends SpecBase with Matchers {

  // Ior[TURN, EORINumber]

  "when EnrolmentId contains only a TURN" - {

    val enrolmentId: EnrolmentId = EnrolmentId(Ior.left(TURN("ABC")))

    "customerId returns the TURN value" in Seq("ABC", "DEF").foreach {
      arbitaryTurn =>
        val enrolmentId: EnrolmentId = EnrolmentId(Ior.left(TURN(arbitaryTurn)))
        enrolmentId.customerId mustEqual arbitaryTurn
    }

    "returns that the enrolment is not modern" in {
      enrolmentId.isModern mustEqual false
    }

  }

  "when EnrolmentId contains only a EORINumber" - {

    val enrolmentId: EnrolmentId = EnrolmentId(Ior.right(EORINumber("ABC")))

    "customerId returns the EORINumber value" in Seq("ABC", "DEF").foreach {
      arbitaryTurn =>
        val enrolmentId: EnrolmentId = EnrolmentId(Ior.right(EORINumber(arbitaryTurn)))
        enrolmentId.customerId mustEqual arbitaryTurn
    }

    "returns that the enrolment is modern" in {
      enrolmentId.isModern mustEqual true
    }
  }

  "when EnrolmentId contains a EORINumber and a TURN return the EORINumber" - {

    val enrolmentId: EnrolmentId = EnrolmentId(Ior.both(TURN("ABC"), EORINumber("DEF")))

    "returns that the enrolment is modern" in {
      enrolmentId.isModern mustEqual true
    }

  }

}
