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
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
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
import play.api.test.Helpers._
import repositories.DepartureRepository
import repositories.LockRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments

import scala.concurrent.Future

class AuthenticatedGetDepartureForWriteActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(authLockAndGet: AuthenticatedGetDepartureForWriteActionProvider) {

    def get(departureId: DepartureId): Action[AnyContent] = authLockAndGet(departureId) {
      request =>
        Results.Ok(request.departure.departureId.toString)
    }

    def failingAction(departureId: DepartureId): Action[AnyContent] = authLockAndGet(departureId).async {
      _ =>
        Future.failed(new Exception)
    }
  }

  "authenticated get departure for write" - {

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

      "must lock, get and unlock a departure when it exists and its EORI matches the user's" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe OK
        contentAsString(result) mustBe departure.departureId.toString
        verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
        verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
      }

      "must lock and unlock a departure and return Not Found when its EORI does not match the user's" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = "invalid EORI number")

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
        verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
        verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
      }

      "must lock, unlock and return Not Found when the departure does not exist" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any())) thenReturn Future.successful(None)
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
        verify(mockLockRepository, times(1)).lock(eqTo(departureId))
        verify(mockLockRepository, times(1)).unlock(eqTo(departureId))
      }

      "must unlock the departure and return Internal Server Error if the main action fails" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.failingAction(departure.departureId)(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
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

      "must lock and unlock a departure and return Forbidden" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe FORBIDDEN
        verify(mockLockRepository, times(1)).lock(eqTo(departureId))
        verify(mockLockRepository, times(1)).unlock(eqTo(departureId))
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockLockRepository = mock[LockRepository]

        when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

        val application = new GuiceApplicationBuilder()
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureForWriteActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe LOCKED
      }
    }
  }
}
