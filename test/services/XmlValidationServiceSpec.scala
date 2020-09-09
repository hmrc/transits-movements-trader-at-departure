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

import base.SpecBase
import models.XSDFile

class XmlValidationServiceSpec extends SpecBase {

  private val xmlValidationService: XmlValidationService = new XmlValidationService()

  object SimpleXSD extends XSDFile("/xsd/simple.xsd")

  "validate" - {

    "must return a success when given a valid xml" in {

      val validXml = <messages>
        <message>
          <testString>Goods are ready to be released</testString>
          <testNumber>1</testNumber>
        </message>
      </messages>

      val result = xmlValidationService.validate(validXml.toString, SimpleXSD)

      result.isSuccess mustBe true
    }

    "must return a failure when given an xml with missing mandatory elements" in {

      val invalidXml = <messages></messages>

      val result = xmlValidationService.validate(invalidXml.toString, SimpleXSD)

      result.isFailure mustBe true
    }

    "must return a failure when given an xml with invalid fields" in {

      val invalidXml = <messages>
        <message>
          <testString>Goods are ready to be released</testString>
          <testNumber>somestring</testNumber>
        </message>
      </messages>

      val result = xmlValidationService.validate(invalidXml.toString, SimpleXSD)

      result.isFailure mustBe true
    }
  }

}
