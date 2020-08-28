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

package services

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import cats.data.NonEmptyList
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageSender
import models.MessageStatus
import models.MessageType
import models.MessageWithStatus
import models.MessageStatus.SubmissionPending
import org.mockito.Mockito.when
import org.scalatest.concurrent.IntegrationPatience
import play.api.inject.bind
import repositories.DepartureIdRepository
import utils.Format

import scala.concurrent.Future

class DepartureServiceSpec extends SpecBase with IntegrationPatience {

  "createDeparture" - {
    "creates a departure declaration with an internal ref number and a mrn, date and time of creation from the message submitted with a message id of 1 and next correlation id of 2" in {
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)
      val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)

      val id   = DepartureId(1)
      val ref  = "ref"
      val eori = "eoriNumber"

      val mockArrivalIdRepository = mock[DepartureIdRepository]
      when(mockArrivalIdRepository.nextId).thenReturn(Future.successful(id))
      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureIdRepository].toInstance(mockArrivalIdRepository)
        )
        .build()

      val service = application.injector.instanceOf[DepartureService]

      val inputMovement =
        <CC015B>
        <SynVerNumMES2>123</SynVerNumMES2>
        <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        <HEAHEA>
          <RefNumHEA4>{ref}</RefNumHEA4>
        </HEAHEA>
      </CC015B>

      val savedMovement =
        <CC015B>
        <SynVerNumMES2>123</SynVerNumMES2><MesSenMES3>{MessageSender(id, 1).toString}</MesSenMES3>
        <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        <HEAHEA>
          <RefNumHEA4>{ref}</RefNumHEA4>
        </HEAHEA>
      </CC015B>

      val expectedDeparture = Departure(
        departureId = id,
        movementReferenceNumber = None,
        referenceNumber = ref,
        eoriNumber = eori,
        status = DepartureStatus.Initialized,
        created = dateTime,
        updated = dateTime,
        messages = NonEmptyList.one(
          MessageWithStatus(dateTime, MessageType.DepartureDeclaration, savedMovement, MessageStatus.SubmissionPending, 1)
        ),
        nextMessageCorrelationId = 2
      )

      val result = service.createDeparture(eori, inputMovement).futureValue

      result mustEqual Right(expectedDeparture)
    }

    "returns Left when the root node is not <CC007A>" in {

      val ref        = "ref"
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[DepartureService]

      val invalidPayload =
        <Foo>
          <SynVerNumMES2>123</SynVerNumMES2>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <DocNumHEA5>{ref}</DocNumHEA5>
          </HEAHEA>
        </Foo>

      service.createDeparture(eori, invalidPayload).futureValue.isLeft mustBe true
    }
  }

  "makeMessageWithStatus" - {

    "returns a message with the Departure Declaration xml payload" in {

      val dateOfPrep  = LocalDate.now()
      val timeOfPrep  = LocalTime.of(1, 1)
      val ref         = "ref"
      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[DepartureService]

      val id = DepartureId(1)

      val movement =
        <CC015B>
          <SynVerNumMES2>123</SynVerNumMES2>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <RefNumHEA4>{ref}</RefNumHEA4>
          </HEAHEA>
        </CC015B>

      val savedMovement =
        <CC015B>
          <SynVerNumMES2>123</SynVerNumMES2><MesSenMES3>{MessageSender(id, 1).toString}</MesSenMES3>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          <HEAHEA>
            <RefNumHEA4>{ref}</RefNumHEA4>
          </HEAHEA>
        </CC015B>

      val messageCorrelationId = 1
      val expectedMessage =
        MessageWithStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DepartureDeclaration, savedMovement, SubmissionPending, messageCorrelationId)

      val result = service.makeMessageWithStatus(id, messageCorrelationId, MessageType.DepartureDeclaration)(movement)
      result mustEqual Right(expectedMessage)
    }

    "does not return a message when the root node does not match the message type" in {

      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val application = baseApplicationBuilder.build()

      val service = application.injector.instanceOf[DepartureService]

      val movement =
        <Foo>
          <SynVerNumMES2>123</SynVerNumMES2>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </Foo>

      val result = service.makeMessageWithStatus(DepartureId(1), 1, MessageType.DepartureDeclaration)(movement)
      result.isLeft mustBe true
    }
  }

}
