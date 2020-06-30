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

package controllers.actions

import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{Action, AnyContent, AnyContentAsEmpty, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.{AuthConnector, Enrolment, EnrolmentIdentifier, Enrolments}

import scala.concurrent.Future

class AuthenticateActionProviderSpec extends FreeSpec with MustMatchers with MockitoSugar {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  def baseApplication: GuiceApplicationBuilder = new GuiceApplicationBuilder()
    .configure("metrics.jvm" -> false)

  class Harness(authenticate: AuthenticateActionProvider) {

    def action(): Action[AnyContent] = authenticate() {
      result =>
        Results.Ok(result.eoriNumber)
    }
  }

  "authenticate" - {

    "when a user has valid enrolments" - {

      val eoriNumber = "EORI"

      val validEnrolments: Enrolments = Enrolments(
        Set(
          Enrolment(
            key = "IR-SA",
            identifiers = Seq(
              EnrolmentIdentifier(
                "UTR",
                "123"
              )
            ),
            state = "Activated"
          ),
          Enrolment(
            key = "HMCE-NCTS-ORG",
            identifiers = Seq(
              EnrolmentIdentifier(
                "VATRegNoTURN",
                eoriNumber
              )
            ),
            state = "Activated"
          )
        )
      )

      "must pass on the user's EORI" in {

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = baseApplication
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )
          .build()

        val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.action()(fakeRequest)

        status(result) mustEqual OK
        contentAsString(result) mustEqual eoriNumber
      }
    }

    "when a user has invalid enrolments" - {

      val invalidEnrolments = Enrolments(
        Set(
          Enrolment(
            key = "IR-SA",
            identifiers = Seq(
              EnrolmentIdentifier(
                "UTR",
                "123"
              )
            ),
            state = "Activated"
          )
        )
      )

      "must return Forbidden" in {

        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = baseApplication
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector.instanceOf[AuthenticateActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.action()(fakeRequest)

        status(result) mustBe FORBIDDEN
      }
    }
  }
}
