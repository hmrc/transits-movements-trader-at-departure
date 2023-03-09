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

package services

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import base.SpecBase
import models.MessageType
import models.MovementReferenceNumber
import models.ParseError.LocalDateParseFailure
import org.scalatest.EitherValues
import utils.Format

class XmlMessageParserSpec extends SpecBase with EitherValues {

  "dateOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC015B>

      XmlMessageParser.dateOfPrepR(movement) mustEqual Right(dateOfPrep)
    }

    "will return a LocalDateParseFailure when the date is longer than 8 with a specific message" in {
      val movement =
        <CC015B>
          <DatOfPreMES9>202105051</DatOfPreMES9>
        </CC015B>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a LocalDateParseFailure when the date is shorter than 6" in {
      val movement =
        <CC015B>
          <DatOfPreMES9>20201</DatOfPreMES9>
        </CC015B>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a LocalDateParseFailure when the date is 7 digits" in {
      val movement =
        <CC015B>
          <DatOfPreMES9>2020121</DatOfPreMES9>
        </CC015B>

      val result = XmlMessageParser.dateOfPrepR(movement).left.get
      result mustBe an[LocalDateParseFailure]
      result.message mustBe "The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"
    }

    "will return a Left when the date in the DatOfPreMES9 node is missing" in {
      val message =
        <CC015B>
        </CC015B>

      val result = XmlMessageParser.dateOfPrepR(message)
      result.isLeft mustEqual true
    }

  }

  "timeOfPrepR" - {
    "returns the time from the TimOfPreMES10 node" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val message =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.timeOfPrepR(message) mustEqual Right(timeOfPrep)
    }

    "returns a Left if TimOfPreMES10 is malformed" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val message =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "a"}</TimOfPreMES10>
        </CC015B>

      val result = XmlMessageParser.timeOfPrepR(message)
      result.isLeft mustBe true
    }

    "returns a Left(LocalTimeParseFailure) if TimOfPreMES10 is missing" in {
      val message =
        <CC015B>
        </CC015B>

      val result = XmlMessageParser.timeOfPrepR(message)
      result.isLeft mustBe true
    }

  }
  "correctRootNodeR" - {
    "returns Right if the root node is as expected" in {

      val movement =
        <CC015B></CC015B>

      XmlMessageParser.correctRootNodeR(MessageType.DepartureDeclaration)(movement).isRight mustBe true
    }

    "returns Left if the root node is not as expected" in {

      val movement =
        <Foo></Foo>

      XmlMessageParser.correctRootNodeR(MessageType.DepartureDeclaration)(movement).isLeft mustBe true
    }
  }

  "dateTimeOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.dateTimeOfPrepR(movement) mustEqual Right(LocalDateTime.of(dateOfPrep, timeOfPrep))

    }

    "will return a Left when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      val result = XmlMessageParser.dateTimeOfPrepR(movement)
      result.isLeft mustBe true
    }

    "will return a Left when the time in the TimOfPreMES10 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "1"}</TimOfPreMES10>
        </CC015B>

      val result = XmlMessageParser.dateTimeOfPrepR(movement)
      result.isLeft mustBe true
    }

    "will return a Left when the date in the DatOfPreMES9 node is missing" in {

      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      val result = XmlMessageParser.dateTimeOfPrepR(movement)
      result.isLeft mustBe true
    }

    "will return a Left when the date in the TimOfPreMES10 node is missing" in {

      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC015B>

      val result = XmlMessageParser.dateTimeOfPrepR(movement)
      result.isLeft mustBe true
    }

  }

  "mrnR" - {
    "returns the mrn from the DocNumHEA5 node" in {
      val mrn = MovementReferenceNumber("MRN")

      val movement =
        <CC007A>
          <HEAHEA>
            <DocNumHEA5>{mrn.value}</DocNumHEA5>
          </HEAHEA>
        </CC007A>

      XmlMessageParser.mrnR(movement) mustEqual Right(mrn)

    }

    "returns Left if DocNumHEA5 node is missing" in {
      val movement =
        <CC007A>
          <HEAHEA>
          </HEAHEA>
        </CC007A>

      val result = XmlMessageParser.mrnR(movement)
      result.isLeft mustBe true
    }

  }

}
