/*
 * Copyright 2020 HM Revenue & Customs
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
import generators.ModelGenerators
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DepartureStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "Initialized must transition" - {

    "to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual DepartureStatus.DepartureSubmitted
    }

    "to DepartureAccepted when receiving a DepartureAccepted event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }

    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "DepartureSubmitted must transition" - {
    "to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual DepartureStatus.DepartureSubmitted
    }

    "to DepartureAccepted when receiving a DepartureAccepted event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }

    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "DepartureAccepted must transition" - {
    "to DepartureAccepted when receiving a DepartureAccepted event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }
  }

  "DepartureRejected must transition" - {
    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureRejected.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "DepartureStatus.values must contain" - {
    "Initialized" in { DepartureStatus.values.contains(DepartureStatus.Initialized) }
    "DepartureSubmitted" in { DepartureStatus.values.contains(DepartureStatus.DepartureSubmitted) }
    "DepartureAccepted" in { DepartureStatus.values.contains(DepartureStatus.MrnAllocated) }
    "DepartureRejected" in { DepartureStatus.values.contains(DepartureStatus.DepartureRejected) }
  }

}
