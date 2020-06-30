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
import models.MessageType
import utils.Format

class XmlMessageParserSpec extends SpecBase {

  "dateOfPrepR" - {
    "returns the date from the DatOfPreMES9 node" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC015B>

      XmlMessageParser.dateOfPrepR(movement).value mustEqual dateOfPrep

    }

    "will return a None when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()

      val message =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
        </CC015B>

      XmlMessageParser.dateOfPrepR(message) must not be (defined)

    }

    "will return a None when the date in the DatOfPreMES9 node is missing" in {
      val message =
        <CC015B>
        </CC015B>

      XmlMessageParser.dateOfPrepR(message) must not be (defined)
    }

  }

  "timeOfPrepR" - {
    "returns the time from the TimOfPreMES10 node" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val message =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.timeOfPrepR(message).value mustEqual timeOfPrep

    }

    "returns a None if TimOfPreMES10 is malformed" in {
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val message =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep) ++ "a"}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.timeOfPrepR(message) must not be (defined)

    }

    "returns a None if TimOfPreMES10 is missing" in {
      val message =
        <CC015B>
        </CC015B>

      XmlMessageParser.timeOfPrepR(message) must not be (defined)

    }

  }

  "referenceR" - {
    "returns the reference from the DocNumHEA5 node" in {
      val reference = "ref"

      val message =
        <CC015B>
          <HEAHEA>
            <RefNumHEA4>{reference}</RefNumHEA4>
          </HEAHEA>
        </CC015B>

      XmlMessageParser.referenceR(message).value mustEqual reference

    }

    "returns None if RefNumHEA4 node is missing" in {
      val message =
        <CC015B>
          <HEAHEA>
          </HEAHEA>
        </CC015B>

      XmlMessageParser.referenceR(message) must not be (defined)

    }

  }

  "correctRootNodeR" - {
    "returns true if the root node is as expected" in {

      val movement =
        <CC015B></CC015B>

      XmlMessageParser.correctRootNodeR(MessageType.DepartureDeclaration)(movement) mustBe (defined)
    }

    "returns false if the root node is not as expected" in {

      val movement =
        <Foo></Foo>

      XmlMessageParser.correctRootNodeR(MessageType.DepartureDeclaration)(movement) must not be defined
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

      XmlMessageParser.dateTimeOfPrepR(movement).value mustEqual LocalDateTime.of(dateOfPrep, timeOfPrep)

    }

    "will return a None when the date in the DatOfPreMES9 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep) ++ "1"}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.dateTimeOfPrepR(movement) must not be (defined)
    }

    "will return a None when the date in the DatOfPreMES10 node is malformed" in {
      val dateOfPrep: LocalDate = LocalDate.now()
      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)  ++ "1" }</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.dateTimeOfPrepR(movement) must not be (defined)
    }

    "will return a None when the date in the DatOfPreMES9 node is missing" in {

      val timeOfPrep: LocalTime = LocalTime.of(1, 1)

      val movement =
        <CC015B>
          <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
        </CC015B>

      XmlMessageParser.dateTimeOfPrepR(movement) must not be (defined)

    }

    "will return a None when the date in the DatOfPreMES10 node is missing" in {

      val dateOfPrep: LocalDate = LocalDate.now()

      val movement =
        <CC015B>
          <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
        </CC015B>

      XmlMessageParser.dateTimeOfPrepR(movement) must not be (defined)

    }

  }

}
