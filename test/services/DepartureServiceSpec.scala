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

package services

import base.SpecBase
import cats.data.Ior
import cats.data.NonEmptyList
import models.ChannelType.Api
import models.MessageStatus.SubmissionPending
import models._
import org.mockito.Mockito.when
import org.scalatest.StreamlinedXmlEquality
import org.scalatest.concurrent.IntegrationPatience
import play.api.inject.bind
import repositories.DepartureIdRepository
import utils.Format
import utils.JsonHelper

import java.time.Clock
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import scala.concurrent.Future

class DepartureServiceSpec extends SpecBase with JsonHelper with IntegrationPatience with StreamlinedXmlEquality {

  "createDeparture" - {
    "creates a departure declaration with an internal ref number and a mrn, date and time of creation from the message submitted with a message id of 1 and next correlation id of 2" in {
      val dateOfPrep     = LocalDate.now()
      val timeOfPrep     = LocalTime.of(1, 1)
      val dateTime       = LocalDateTime.of(dateOfPrep, timeOfPrep)
      implicit val clock = Clock.fixed(dateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

      val id   = DepartureId(1)
      val ref  = "ref"
      val eori = "eoriNumber"

      val mockDepartureIdRepository = mock[DepartureIdRepository]
      when(mockDepartureIdRepository.nextId).thenReturn(Future.successful(id))
      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureIdRepository].toInstance(mockDepartureIdRepository),
          bind[Clock].toInstance(clock)
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
        channel = Api,
        movementReferenceNumber = None,
        referenceNumber = ref,
        eoriNumber = eori,
        created = dateTime,
        lastUpdated = dateTime,
        messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            dateTime,
            MessageType.DepartureDeclaration,
            savedMovement,
            MessageStatus.SubmissionPending,
            1,
            convertXmlToJson(savedMovement.toString)
          )
        ),
        nextMessageCorrelationId = 2,
        notificationBox = None
      )

      val result = service.createDeparture(Ior.right(EORINumber(eori)), inputMovement, Api, None).futureValue

      result.right.get mustEqual expectedDeparture
    }

    "returns Left when the root node is not <CC007A>" in {

      val id         = DepartureId(1)
      val ref        = "ref"
      val eori       = "eoriNumber"
      val dateOfPrep = LocalDate.now()
      val timeOfPrep = LocalTime.of(1, 1)

      val mockDepartureIdRepository = mock[DepartureIdRepository]
      when(mockDepartureIdRepository.nextId()).thenReturn(Future.successful(id))

      val application = baseApplicationBuilder
        .overrides(
          bind[DepartureIdRepository].toInstance(mockDepartureIdRepository)
        )
        .build()

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

      service.createDeparture(Ior.right(EORINumber(eori)), invalidPayload, Api, None).futureValue.isLeft mustBe true
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
        MessageWithStatus(
          MessageId(1),
          LocalDateTime.of(dateOfPrep, timeOfPrep),
          MessageType.DepartureDeclaration,
          savedMovement,
          SubmissionPending,
          messageCorrelationId,
          convertXmlToJson(savedMovement.toString)
        )

      val result = service.makeMessageWithStatus(id, expectedMessage.messageId, messageCorrelationId, MessageType.DepartureDeclaration)(movement)
      result.right.get mustEqual expectedMessage
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

      val result = service.makeMessageWithStatus(DepartureId(1), MessageId(1), 1, MessageType.DepartureDeclaration)(movement)
      result.isLeft mustBe true
    }
  }
}
