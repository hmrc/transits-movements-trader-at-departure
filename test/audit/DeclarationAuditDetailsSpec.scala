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

package audit

import base.SpecBase
import cats.data.Ior
import generators.ModelGenerators
import models.ChannelType
import models.MessageWithStatus
import models.MovementReferenceNumber
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import models.EORINumber
import config.Constants
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import utils.XMLTransformer.toJson
import utils.Format
import utils.MessageTranslation

import java.time.LocalDate
import java.time.LocalTime
import scala.xml.NodeSeq

class DeclarationAuditDetailsSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach with ModelGenerators {

  val mockMessageTranslation: MessageTranslation = mock[MessageTranslation]

  "DeclarationAuditDetails" - {

    def gen(xml: NodeSeq) =
      for {
        message <- arbitrary[MessageWithStatus]
      } yield message.copy(message = xml)

    val xml =
      <CC015B>
        <SynVerNumMES2>123</SynVerNumMES2>
        <DatOfPreMES9>
          {Format.dateFormatted(LocalDate.now())}
        </DatOfPreMES9>
        <TimOfPreMES10>
          {Format.timeFormatted(LocalTime.of(1, 1))}
        </TimOfPreMES10>
        <HEAHEA>
          <RefNumHEA4>
            {arbitrary[MovementReferenceNumber].sample.value}
          </RefNumHEA4>
          <TRACONCO1>consignor</TRACONCO1>
          <TRACONCE1>consignee</TRACONCE1>
          <TRAPRIPC1>trader</TRAPRIPC1>
          <GuaRefNumGRNREF1>guaranteeRefNumber</GuaRefNumGRNREF1>
          <AccCodREF6>accessCode</AccCodREF6>
          <CUSOFFTRARNS>trasitOffice</CUSOFFTRARNS>
          <SEAIDSID>sealID</SEAIDSID>
          <GUAREFREF>guarantee</GUAREFREF>
          <TotNumOfIteHEA305>30</TotNumOfIteHEA305>
          <PREADMREFAR2>prevAdminReference</PREADMREFAR2>
          <PRODOCDC2>producedDocs</PRODOCDC2>
          <PRODOCDC2>producedDocs</PRODOCDC2>
          <SPEMENMT2>specialMention</SPEMENMT2>
          <SPEMENMT2>specialMention</SPEMENMT2>
          <SPEMENMT2>specialMention</SPEMENMT2>
        </HEAHEA>
      </CC015B>

    val message     = gen(xml).sample.get
    val enrolmentId = Ior.right(EORINumber(Constants.NewEnrolmentIdKey))

    val statistics = (requestLength: Int) =>
      Json.obj(
        "consignor1"                -> "consignor",
        "consignor2"                       -> "NULL",
        "consignee1"                       -> "consignee",
        "consignee2"                       -> "NULL",
        "principalTrader"                  -> "trader",
        "guaranteeReferenceNumber"         -> "guaranteeRefNumber",
        "guaranteeAccessCode"              -> "accessCode",
        "packageCount"                     -> "NULL",
        "itemCount"                        -> "30",
        "totalNoOfTransitOffices"          -> 1,
        "totalNoOfSeals"                   -> 1,
        "totalNoOfGuarantees"              -> 1,
        "totalNoOfPreviousAdminReferences" -> 1,
        "totalNoOfProducedDocs"            -> 2,
        "totalNoOfSpecialMentions"         -> 3,
        "totalNoOfContainers"              -> 0,
        "totalNoOfCountriesOfRouting"      -> 0,
        "requestLength"                    -> requestLength
    )

    "must include translated xml when request size is less than max size allowed and generate xml statistics" in {

      val requestLength = DeclarationAuditDetails.maxRequestLength - 1000

      val application = baseApplicationBuilder
        .overrides(bind[MessageTranslation].toInstance(mockMessageTranslation))
        .build()
      val messageTranslation = application.injector.instanceOf[MessageTranslation]
      val jsonMessage        = messageTranslation.translate(toJson(message.message))

      val expectedDetails = Json.obj(
        "channel"       -> "api",
        "customerId"    -> "EORINumber",
        "enrolmentType" -> "HMRC-CTC-ORG",
        "message"       -> jsonMessage,
        "statistics"    -> statistics(requestLength)
      )

      val details = DeclarationAuditDetails(ChannelType.Api, enrolmentId, message.message, requestLength, mockMessageTranslation)

      Json.toJson(details).as[JsObject] mustEqual expectedDetails
    }

    "must include message to indicate request size is more than max size allowed and generate xml statistics" in {

      val requestLength = DeclarationAuditDetails.maxRequestLength + 1000

      val expectedDetails = Json.obj(
        "channel"       -> "api",
        "customerId"    -> "EORINumber",
        "enrolmentType" -> "HMRC-CTC-ORG",
        "message"       -> Json.obj("declaration" -> "Departure declaration too large to be included"),
        "statistics"    -> statistics(requestLength)
      )

      val details = DeclarationAuditDetails(ChannelType.Api, enrolmentId, message.message, requestLength, mockMessageTranslation)

      Json.toJson(details).as[JsObject] mustEqual expectedDetails
    }
  }
}
