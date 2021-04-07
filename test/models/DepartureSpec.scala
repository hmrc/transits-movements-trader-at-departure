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

package models

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDateTime

class DepartureSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  val departureGenerator: Gen[Departure] =
    for {
      messages  <- nonEmptyListOfMaxLength[MessageWithStatus](20)
      departure <- arbitrary[Departure].map(_.copy(messages = messages))
    } yield departure

  "nextMessageId returns a MessageId which has value that is 1 larger than the number of messages" in {
    forAll(departureGenerator) {
      departure =>
        (MessageId.unapply(departure.nextMessageId).value - departure.messages.length) mustEqual 1
    }
  }

  "messageWithId returns a list with the message and the MessageId whose value is one more than the index" in {
    forAll(departureGenerator) {
      departure =>
        departure.messagesWithId.zipWithIndex.toList.foreach {
          case ((message, messageId), index) =>
            message mustEqual departure.messages.toList(index)
            (MessageId.unapply(messageId).value - index) mustEqual 1
        }
    }
  }

  "latestMRNAllocatedMessage" - {
    "return the latest mrn allocated message if multiple exist" in {
      val now       = LocalDateTime.now()
      val departure = departureGenerator.sample.value

      val messages = NonEmptyList[Message](
        MessageWithStatus(now, MessageType.DepartureDeclaration, <one>one</one>, MessageStatus.SubmissionSucceeded, 1),
        MessageWithoutStatus(now, MessageType.MrnAllocated, <two>two</two>, 4) ::
          MessageWithoutStatus(now, MessageType.PositiveAcknowledgement, <three>three</three>, 3) ::
          MessageWithoutStatus(now, MessageType.MrnAllocated, <four>four</four>, 2) :: Nil,
      )

      departure.copy(messages = messages).latestMRNAllocatedMessage.value mustBe <two>two</two>
    }
    "return the mrn allocated message if one exist" in {
      val now       = LocalDateTime.now()
      val departure = departureGenerator.sample.value

      val messages = NonEmptyList[Message](
        MessageWithStatus(now, MessageType.DepartureDeclaration, <one>one</one>, MessageStatus.SubmissionSucceeded, 1),
        MessageWithoutStatus(now, MessageType.MrnAllocated, <seven>two</seven>, 2) ::
          MessageWithoutStatus(now, MessageType.PositiveAcknowledgement, <three>three</three>, 3) :: Nil,
      )

      departure.copy(messages = messages).latestMRNAllocatedMessage.value mustBe <seven>two</seven>
    }
    "return None if no MrnAllocated message exist" in {
      val now       = LocalDateTime.now()
      val departure = departureGenerator.sample.value

      val messages = NonEmptyList[Message](
        MessageWithStatus(now, MessageType.DepartureDeclaration, <one>one</one>, MessageStatus.SubmissionSucceeded, 1),
        MessageWithoutStatus(now, MessageType.PositiveAcknowledgement, <two>two</two>, 2) :: Nil,
      )

      departure.copy(messages = messages).latestMRNAllocatedMessage mustBe None
    }
  }

}
