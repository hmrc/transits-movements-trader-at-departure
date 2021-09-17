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

package audit

import audit.AuditType._
import base.SpecBase
import cats.data.Ior
import generators.ModelGenerators
import models.CancellationDecisionResponse
import models.ChannelType.Api
import models.ControlDecisionNotificationResponse
import models.Departure
import models.DepartureRejectedResponse
import models.EORINumber
import models.GuaranteeNotValidResponse
import models.MessageWithStatus
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import models.WriteOffNotificationResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.test.Helpers.running
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import scala.xml.NodeSeq

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach with ModelGenerators {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  "AuditService" - {
    "must audit notification message event" in {

      def gen(xml: NodeSeq, jsObj: JsObject): Gen[MessageWithStatus] =
        for {
          message <- arbitrary[MessageWithStatus]
        } yield message.copy(message = xml, messageJson = jsObj)

      val requestEori        = Ior.right(EORINumber("eori"))
      val requestXml         = <xml>test</xml>
      val requestedXmlToJson = Json.obj("channel" -> "api", "xml" -> "test")
      val message            = gen(requestXml, requestedXmlToJson).sample.get

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
  }

}
