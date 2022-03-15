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
import models.ChannelType.Api
import models.CancellationDecisionResponse
import models.ChannelType
import models.ControlDecisionNotificationResponse
import models.Departure
import models.DepartureId
import models.DepartureRejectedResponse
import models.EORINumber
import models.GuaranteeNotValidResponse
import models.MessageWithStatus
import models.MovementReferenceNumber
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import models.WriteOffNotificationResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.request.AuthenticatedRequest
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
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

  "AuditService" - {

    def gen(xml: NodeSeq) =
      for {
        message <- arbitrary[MessageWithStatus]
      } yield message.copy(message = xml)

    "must audit notification message event" in {

      val requestEori = Ior.right(EORINumber("eori"))
      val requestXml  = <xml>test</xml>
      val message     = gen(requestXml).sample.get

      forAll(Gen.oneOf(AuditType.values)) {
        auditType =>
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()
          running(application) {
            val auditService = application.injector.instanceOf[AuditService]
            auditService.auditEvent(auditType, requestEori, message, Api)
            verify(mockAuditConnector, times(1)).sendExplicitAudit[AuditDetails](eqTo(auditType.toString()), any())(any(), any(), any())
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

            verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(nctsAuditResponse(response).toString()), any[AuditDetails]())(any(), any(), any())
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
        (channel) =>
          val request = new AuthenticatedRequest[Any](FakeRequest(), channel, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))
          val application = baseApplicationBuilder
            .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
            .build()

          running(application) {
            val auditService    = application.injector.instanceOf[AuditService]
            val departureId     = DepartureId(1234)
            val expectedDetails = AuthenticatedAuditDetails(request.channel, request.enrolmentId, Json.obj("departureId" -> departureId))
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

    "must adjust declaration audit according to request size" - {

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
            <TotNumOfIteHEA305>1</TotNumOfIteHEA305>
          </HEAHEA>
        </CC015B>

      val statistics = (contentLength: Int) =>
        Json.obj(
          "Total number of items"              -> 1,
          "Declaration request content-length" -> contentLength
      )

      val message = gen(xml).sample.get

      "when the request is smaller than max size allowed the xml and statistics should be included in the audit detail" in {

        val contentLength = 1000
        val request       = new AuthenticatedRequest[Any](FakeRequest(), Api, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

        val application = baseApplicationBuilder
          .configure("message-translation-file" -> "MessageTranslation.json")
          .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
          .build()

        running(application) {
          val auditService       = application.injector.instanceOf[AuditService]
          val messageTranslation = application.injector.instanceOf[MessageTranslation]

          val expectedDetails = DeclarationAuditDetails(request.channel, request.enrolmentId, contentLength, message.message, messageTranslation)

          auditService.auditDeclarationWithStatistics(contentLength, DepartureDeclarationSubmitted, request.enrolmentId, message, request.channel)

          verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(DepartureDeclarationSubmitted.toString), eqTo(expectedDetails))(any(), any(), any())
          reset(mockAuditConnector)
        }
      }

      "when the request is larger than max size allowed the audit detail should include statistics and replace translated xml with message" in {

        val contentLength = 60000
        val request       = new AuthenticatedRequest[Any](FakeRequest(), Api, Ior.right(EORINumber(Constants.NewEnrolmentIdKey)))

        val json = Json.obj(
          "Declaration Data" -> "The declaration data was too large to be included",
        )

        val application = baseApplicationBuilder
          .configure("message-translation-file" -> "MessageTranslation.json")
          .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
          .build()
        running(application) {

          val messageTranslation = application.injector.instanceOf[MessageTranslation]
          val expectedDetails    = DeclarationAuditDetails(request.channel, request.enrolmentId, contentLength, message.message, messageTranslation)

          val auditService = application.injector.instanceOf[AuditService]
          auditService.auditDeclarationWithStatistics(contentLength, DepartureDeclarationSubmitted, request.enrolmentId, message, request.channel)

          verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(DepartureDeclarationSubmitted.toString), eqTo(expectedDetails))(any(), any(), any())
          reset(mockAuditConnector)
        }
      }
    }
  }
}
