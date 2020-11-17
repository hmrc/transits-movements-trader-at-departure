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

import audit.AuditType._
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
import org.scalacheck.Gen
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

      val auditDetails = Json.toJson(AuditDetails("api", Json.obj("xml" -> "test"), requestXml.toString()))

      forAll(Gen.oneOf(AuditType.values)) {
        auditType =>
          {
            val application = baseApplicationBuilder
              .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
              .build()
            running(application) {
              val auditService = application.injector.instanceOf[AuditService]
              auditService.auditEvent(auditType, requestXml, "api")
              verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(auditType.toString()), eqTo(auditDetails))(any(), any(), any())
              reset(mockAuditConnector)
            }
          }
      }
    }

    "must audit NCTS message response events" in {
      val requestXml = <xml>test</xml>

      val nctsAuditResponse = Map(
        PositiveAcknowledgementResponse     -> PositiveAcknowledgementReceived,
        MrnAllocatedResponse                -> MrnAllocatedReceived,
        DepartureRejectedResponse           -> DeclarationRejectedReceived,
        ControlDecisionNotificationResponse -> ControlDecisionNotificationReceived,
        NoReleaseForTransitResponse         -> NoReleaseForTransitReceived,
        ReleaseForTransitResponse           -> ReleaseForTransitReceived,
        CancellationDecisionResponse        -> CancellationDecisionReceived,
        WriteOffNotificationResponse        -> WriteOffNotificationReceived,
        GuaranteeNotValidResponse           -> GuaranteeNotValidReceived
      )

      forAll(Gen.oneOf(nctsAuditResponse.keys)) {
        response =>
          {
            val application = baseApplicationBuilder
              .overrides(bind[AuditConnector].toInstance(mockAuditConnector))
              .build()

            running(application) {
              val auditService = application.injector.instanceOf[AuditService]

              auditService.auditNCTSMessages(response, requestXml)

              verify(mockAuditConnector, times(1)).sendExplicitAudit(eqTo(nctsAuditResponse(response).toString()), any[AuditDetails]())(any(), any(), any())
              reset(mockAuditConnector)
            }
          }
      }
    }
  }

}
