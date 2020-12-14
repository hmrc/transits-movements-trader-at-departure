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

import generators.ModelGenerators
import models.Departure
import models.DepartureId
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers.contentAsString
import play.api.test.Helpers.status
import play.api.test.Helpers._
import repositories.DepartureRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments

import scala.concurrent.Future

class AuthenticateGetDepartureForReadActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  val applicationBuilder = new GuiceApplicationBuilder()
    .configure("metrics.jvm" -> false)

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(authAndGet: AuthenticatedGetDepartureForReadActionProvider) {

    def get(departureId: DepartureId): Action[AnyContent] = authAndGet(departureId) {
      request =>
        Results.Ok(request.departure.departureId.toString)
    }
  }

  "authenticated get departure for read" - {

    "when given valid enrolments" - {

      val eoriNumber = "123"

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

      "must get a departure when it exists and its EORI matches the user's" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(Some(departure))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe OK
        contentAsString(result) mustBe departure.departureId.toString
      }

      "must return Not Found when the departure exists and its EORI does not match the user's" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = "invalid EORI number")

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(Some(departure))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
      }

      "must return Not Found when the departure does not exist" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(None)

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
      }

      "must return InternalServerError when repository has issues" in {
        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.failed(new Exception)

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR

      }
    }

    "when given invalid enrolments" - {

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

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = applicationBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe FORBIDDEN
      }
    }
  }
}
