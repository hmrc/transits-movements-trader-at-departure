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
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsString
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.DepartureRepository
import repositories.LockRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.Future
import scala.xml.NodeSeq

class AuthenticatedGetArrivalForWriteActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[NodeSeq] = FakeRequest("", "").withBody(<CC015B>
    <HEAHEA>
      <RefNumHEA4>ref</RefNumHEA4>
    </HEAHEA>
  </CC015B>)

  def baseApplication: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure("metrics.jvm" -> false)

  class Harness(authOptionalGet: AuthenticateGetOptionalDepartureForWriteActionProvider, cc: ControllerComponents) extends BackendController(cc) {

    def get: Action[NodeSeq] = authOptionalGet()(parse.xml) {
      request =>
        Results.Ok(JsBoolean(request.departure.isDefined))
    }

    def getJson: Action[AnyContent] = authOptionalGet() {
      request =>
        Results.Ok(JsBoolean(request.departure.isDefined))
    }

    def failingAction: Action[NodeSeq] = authOptionalGet()(cc.parsers.xml).async {
      _ =>
        Future.failed(new Exception)
    }
  }

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

  "authenticated get departure for write" - {

    "when given valid enrolments" - {

      "must not lock and does not return a departure when one does not exist" in {

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(None)

        val application = baseApplication
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)

        val result = controller.get()(fakeRequest)
        status(result) mustBe OK
        contentAsJson(result) mustBe JsBoolean(false)
      }

      "must lock, get and unlock a departure when it exists and its EORI matches the user's" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = baseApplication
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )
          .build()

        val actionProvider = application.injector.instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector.instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)
        val result     = controller.get()(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe JsBoolean(true)
        verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
        verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
      }

      "must unlock the departure if we find and lock one and return Internal Server Error if the main action fails" in {

        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockLockRepository               = mock[LockRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
        when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

        val application = baseApplication
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[LockRepository].toInstance(mockLockRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)
        val result     = controller.failingAction()(fakeRequest)

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

      "must return Forbidden" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(invalidEnrolments))

        val application = baseApplication
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)
        val result     = controller.get(fakeRequest)

        status(result) mustBe FORBIDDEN
      }
    }

    "when a lock cannot be acquired" - {

      "must return Locked" in {
        val departure = arbitrary[Departure].sample.value

        val mockLockRepository               = mock[LockRepository]
        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.get(any(), any())) thenReturn Future.successful(Some(departure))
        when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

        val application = baseApplication
          .overrides(
            bind[LockRepository].toInstance(mockLockRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[DepartureRepository].toInstance(mockDepartureRepository)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)
        val result     = controller.get()(fakeRequest)

        status(result) mustBe LOCKED
      }
    }

    "when we can't parse a reference from the body" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = baseApplication
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)

        val result = controller.get()(FakeRequest().withBody(<CC015B>
          <HEAHEA>
          </HEAHEA>
        </CC015B>))

        status(result) mustBe BAD_REQUEST
      }
    }

    "when have something other than xml in the body" - {
      "must return BadRequest" in {
        val mockAuthConnector: AuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = baseApplication
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector)
          )

        val actionProvider = application.injector().instanceOf[AuthenticateGetOptionalDepartureForWriteActionProvider]
        val cc             = application.injector().instanceOf[ControllerComponents]

        val controller = new Harness(actionProvider, cc)

        val request: FakeRequest[AnyContent] = FakeRequest().withBody(AnyContent(JsString("Happy Apples")))

        val result = controller.getJson()(request)

        status(result) mustBe BAD_REQUEST
      }
    }
  }
}
