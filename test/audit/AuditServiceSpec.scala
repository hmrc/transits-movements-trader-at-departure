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

package audit

import base.SpecBase
import models.PositiveAcknowledgementResponse
import models.MrnAllocatedResponse
import models.DepartureRejectedResponse
import models.ControlDecisionNotificationResponse
import models.NoReleaseForTransitResponse
import models.ReleaseForTransitResponse
import models.CancellationDecisionResponse
import models.WriteOffNotificationResponse
import models.GuaranteeNotValidResponse
import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito.reset
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.Helpers.running
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

class AuditServiceSpec extends SpecBase with ScalaCheckPropertyChecks with BeforeAndAfterEach {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuditConnector)
  }

  "AuditService" - {
    "must audit notification message event" in {

      val requestXml = <xml>test</xml>

      val auditType    = "Some AuditEvent"
      val auditDetails = Json.toJson(AuditDetails(Json.obj("xml" -> "test"), requestXml.toString()))

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()
      running(application) {
        val auditService = application.injector.instanceOf[AuditService]
        auditService.auditEvent(auditType, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType), eqTo(auditDetails))(any(), any(), any())
      }
    }

    "must audit NCTS message PositiveAcknowledgementResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(PositiveAcknowledgementResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.PositiveAcknowledgementReceived), any[AuditDetails]())(any(), any(), any())
      }
    }

    "must audit NCTS message MrnAllocatedResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(MrnAllocatedResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.MrnAllocatedReceived), any[AuditDetails]())(any(), any(), any())
      }
    }

    "must audit NCTS message DepartureRejectedResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(DepartureRejectedResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.DeclarationRejectedReceived), any[AuditDetails]())(any(), any(), any())
      }

    }

    "must audit NCTS message ControlDecisionNotificationResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(ControlDecisionNotificationResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.ControlDecisionNotificationReceived), any[AuditDetails]())(any(),
                                                                                                                                              any(),
                                                                                                                                              any())
      }

    }

    "must audit NCTS message NoReleaseForTransitResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(NoReleaseForTransitResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.NoReleaseForTransitReceived), any[AuditDetails]())(any(), any(), any())
      }

    }

    "must audit NCTS message ReleaseForTransitResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(ReleaseForTransitResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.ReleaseForTransitReceived), any[AuditDetails]())(any(), any(), any())
      }

    }

    "must audit NCTS message CancellationDecisionResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(CancellationDecisionResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.CancellationDecisionReceived), any[AuditDetails]())(any(), any(), any())
      }

    }

    "must audit NCTS message WriteOffNotificationResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(WriteOffNotificationResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.WriteOffNotificationReceived), any[AuditDetails]())(any(), any(), any())
      }

    }

    "must audit NCTS message GuaranteeNotValidResponse event" in {
      val requestXml = <xml>test</xml>

      val application = baseApplicationBuilder
        .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
        .build()

      running(application) {
        val auditService = application.injector.instanceOf[AuditService]

        auditService.auditNCTSMessages(GuaranteeNotValidResponse, requestXml)

        verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(AuditType.NCTS.GuaranteeNotValidReceived), any[AuditDetails]())(any(), any(), any())
      }

    }
  }

}
