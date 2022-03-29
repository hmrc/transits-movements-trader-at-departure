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

import audit.AuditType._
import base.SpecBase
import cats.data.Ior
import config.Constants
import generators.ModelGenerators
import models.CancellationDecisionResponse
import models.ChannelType
import models.ControlDecisionNotificationResponse
import models.Departure
import models.DepartureId
import models.DepartureRejectedResponse
import models.EORINumber
import models.EnrolmentId
import models.GuaranteeNotValidResponse
import models.MessageWithStatus
import models.MovementReferenceNumber
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import models.WriteOffNotificationResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.ChannelType.Api
import models.request.AuthenticatedRequest
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.bind
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.running
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.Format
import utils.MessageTranslation
import utils.XMLTransformer.toJson

import java.time.LocalDate
import java.time.LocalTime
import scala.xml.NodeSeq

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach with ModelGenerators {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  val enrolmentId = EnrolmentId(Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

  def gen(xml: NodeSeq) =
    for {
      message <- arbitrary[MessageWithStatus]
    } yield message.copy(message = xml)

  "AuditService" - {

    def gen(xml: NodeSeq) =
      for {
        message <- arbitrary[MessageWithStatus]
      } yield message.copy(message = xml)

    "must audit notification message event" in {
      val requestXml = <xml>test</xml>
      val message    = gen(requestXml).sample.get

      forAll(Gen.oneOf(AuditType.values)) {
        auditType =>
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()
          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            auditService.auditEvent(auditType, enrolmentId, message, Api)
            verify(mockAuditConnector, times(1)).sendExplicitAudit[AuditDetails](eqTo(auditType.toString), any())(any(), any(), any())
            reset(mockAuditConnector)
          }
      }
    }

    "must audit NCTS message response events" in {
      val requestXml = <xml>test</xml>

      val nctsAuditResponse = Map(
        PositiveAcknowledgementResponse              -> PositiveAcknowledgementReceived,
        MrnAllocatedResponse                         -> MrnAllocatedReceived,
        DepartureRejectedResponse                    -> DeclarationRejectedReceived,
        ControlDecisionNotificationResponse          -> ControlDecisionNotificationReceived,
        NoReleaseForTransitResponse                  -> NoReleaseForTransitReceived,
        ReleaseForTransitResponse                    -> ReleaseForTransitReceived,
        CancellationDecisionResponse                 -> CancellationDecisionReceived,
        WriteOffNotificationResponse                 -> WriteOffNotificationReceived,
        GuaranteeNotValidResponse                    -> GuaranteeNotValidReceived,
        XMLSubmissionNegativeAcknowledgementResponse -> XMLSubmissionNegativeAcknowledgement
      )

      forAll(Gen.oneOf(nctsAuditResponse.keys)) {
        response =>
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            val departure    = Arbitrary.arbitrary[Departure].sample.value

            auditService.auditNCTSMessages(departure.channel, departure.eoriNumber, response, requestXml)

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(nctsAuditResponse(response).toString), any[AuditDetails]())(any(), any(), any())
            reset(mockAuditConnector)
          }
      }
    }

    "must audit auth events" in {

      forAll(Gen.oneOf(ChannelType.values), Gen.oneOf(Seq(Constants.LegacyEnrolmentIdKey, Constants.NewEnrolmentIdKey))) {
        (channel, enrolmentType) =>
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            val details      = AuthenticationDetails(channel, enrolmentType)
            auditService.authAudit(SuccessfulAuthTracking, details)

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(SuccessfulAuthTracking.toString), eqTo(details))(any(), any(), any())
            reset(mockAuditConnector)
          }

      }
    }

    "must audit customer missing movement events" in {
      forAll(Gen.oneOf(ChannelType.values)) {
        channel =>
          val request = new AuthenticatedRequest[Any](FakeRequest(), channel, enrolmentId)
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            val departureId  = DepartureId(1234)

            val expectedDetails = AuthenticatedAuditDetails(
              request.channel,
              request.enrolmentId.customerId,
              request.enrolmentId.enrolmentType,
              Json.obj("departureId" -> departureId)
            )

            auditService.auditCustomerRequestedMissingMovementEvent(request, departureId)

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(CustomerRequestedMissingMovement.toString), eqTo(expectedDetails))(any(), any(), any())
            reset(mockAuditConnector)
          }

      }
    }

    "must audit NCTS missing movement events" in {
      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService    = application.injector.instanceOf[AuditService]
        val departureId     = DepartureId(1234)
        val messageResponse = WriteOffNotificationResponse
        val xml             = <node>test</node>
        auditService.auditNCTSRequestedMissingMovementEvent(departureId, messageResponse, xml)

        val expectedDetails = Json.obj(
          "departureId"         -> departureId,
          "messageResponseType" -> WriteOffNotificationReceived.toString,
          "message"             -> toJson(xml)
        )

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(NCTSRequestedMissingMovement.toString), eqTo(expectedDetails))(any(), any())
        reset(mockAuditConnector)

      }
    }

    "must audit departure declaration events" - {

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

      val message = gen(xml).sample.get

      val mockMessageTranslation: MessageTranslation = mock[MessageTranslation]
      when(mockMessageTranslation.translate(any[JsObject])).thenAnswer(_.getArgument[JsObject](0))

      val enrolmentId = EnrolmentId(Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

      val statistics = (requestLength: Int) =>
        Json.obj(
          "consignor1"                       -> "consignor",
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

      Seq(arbitraryBox.arbitrary.sample, None).foreach {
        boxOpt =>
          val withString = boxOpt
            .map(
              _ => "with"
            )
            .getOrElse("without")
          s"$withString a box" - {

            "and must include translated xml when request size is less than max size allowed and generate xml statistics" in {
              val requestLength = AuditService.maxRequestLength - 1000

              val application = baseApplicationBuilder
                .overrides(
                  bind[MessageTranslation].toInstance(mockMessageTranslation),
                  bind[AuditConnector].toInstance(mockAuditConnector)
                )
                .build()

              val jsonMessage = mockMessageTranslation.translate(toJson(message.message))

              val expectedSubmission =
                DeclarationAuditDetails(
                  ChannelType.Api,
                  Constants.NewEnrolmentIdKey,
                  Constants.NewEnrolmentKey,
                  jsonMessage,
                  statistics(requestLength),
                  boxOpt.map(_.boxId)
                )

              running(application) {
                val auditService = application.injector.instanceOf[AuditService]
                auditService.auditDeclarationWithStatistics(
                  DepartureDeclarationSubmitted,
                  enrolmentId.customerId,
                  enrolmentId.enrolmentType,
                  message,
                  ChannelType.Api,
                  requestLength,
                  boxOpt.map(_.boxId)
                )

                verify(mockAuditConnector, times(1))
                  .sendExplicitAudit[DeclarationAuditDetails](eqTo(DepartureDeclarationSubmitted.toString), eqTo(expectedSubmission))(any(), any(), any())
              }
            }

            "and must include message to indicate request size is more than max size allowed and generate xml statistics" in {
              val requestLength = AuditService.maxRequestLength + 1000

              val application = baseApplicationBuilder
                .overrides(
                  bind[MessageTranslation].toInstance(mockMessageTranslation),
                  bind[AuditConnector].toInstance(mockAuditConnector)
                )
                .build()

              val jsonMessage = Json.obj("declaration" -> "Departure declaration too large to be included")

              val expectedSubmission =
                DeclarationAuditDetails(
                  ChannelType.Api,
                  Constants.NewEnrolmentIdKey,
                  Constants.NewEnrolmentKey,
                  jsonMessage,
                  statistics(requestLength),
                  boxOpt.map(_.boxId)
                )

              running(application) {
                val auditService = application.injector.instanceOf[AuditService]
                auditService.auditDeclarationWithStatistics(
                  DepartureDeclarationSubmitted,
                  enrolmentId.customerId,
                  enrolmentId.enrolmentType,
                  message,
                  ChannelType.Api,
                  requestLength,
                  boxOpt.map(_.boxId)
                )

                verify(mockAuditConnector, times(1))
                  .sendExplicitAudit[DeclarationAuditDetails](eqTo(DepartureDeclarationSubmitted.toString), eqTo(expectedSubmission))(any(), any(), any())
              }
            }
          }
      }
    }
  }
}
