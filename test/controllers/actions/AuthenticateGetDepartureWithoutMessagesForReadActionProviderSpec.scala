/*
 * Copyright 2023 HM Revenue & Customs
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
import migrations.FakeMigrationRunner
import migrations.MigrationRunner
import models.DepartureId
import models.DepartureWithoutMessages
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc._
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
import models.ChannelType.Web
import models.ChannelType.Api
import audit.AuditService
import models.request.AuthenticatedRequest

class AuthenticateGetDepartureWithoutMessagesForReadActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  val applicationBuilder = new GuiceApplicationBuilder()
    .configure("metrics.jvm" -> false)

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "").withHeaders("channel" -> Web.toString())

  class Harness(authAndGet: AuthenticatedGetDepartureWithoutMessagesForReadActionProvider) {

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

        val departure = arbitrary[DepartureWithoutMessages].sample.value copy (eoriNumber = eoriNumber)

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(Some(departure))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe OK
        contentAsString(result) mustBe departure.departureId.toString
      }

      "must return Not Found when the departure exists and its EORI does not match the user's" in {

        val departure = arbitrary[DepartureWithoutMessages].sample.value copy (eoriNumber = "invalid EORI number")

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(Some(departure))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departure.departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
      }

      "must return Not Found and audit when the departure does not exist" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]
        val mockAuditService: AuditService   = mock[AuditService]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), any())) thenReturn Future.successful(None)

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner],
            bind[AuditService].toInstance(mockAuditService)
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe NOT_FOUND
        verify(mockAuditService, times(1)).auditCustomerRequestedMissingMovementEvent(any[AuthenticatedRequest[_]], eqTo(departureId))
      }

      "must return Not Found when the departure exists but does not share the channel" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), eqTo(Api))).thenReturn(Future.successful(None))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

          val fakeAPIRequest = fakeRequest.withHeaders("channel" -> Api.toString())

          val controller = new Harness(actionProvider)
          val result     = controller.get(departureId)(fakeAPIRequest)

          status(result) mustBe NOT_FOUND
        }
      }

      "must return Ok when the departure exists and shares the same channel" in {

        val departure = arbitrary[DepartureWithoutMessages].sample.value copy (eoriNumber = eoriNumber, channel = Api)

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), eqTo(Api))).thenReturn(Future.successful(Some(departure)))

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

          val fakeAPIRequest = fakeRequest.withHeaders("channel" -> departure.channel.toString())

          val controller = new Harness(actionProvider)
          val result     = controller.get(departureId)(fakeAPIRequest)

          status(result) mustBe OK
        }
      }

      "must return InternalServerError when repository has issues" in {
        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector: AuthConnector = mock[AuthConnector]
        val mockDepartureRepository          = mock[DepartureRepository]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))
        when(mockDepartureRepository.getWithoutMessages(any(), any())) thenReturn Future.failed(new Exception)

        val application = applicationBuilder
          .overrides(
            bind[DepartureRepository].toInstance(mockDepartureRepository),
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

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
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )

        val actionProvider = application.injector().instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

        val controller = new Harness(actionProvider)
        val result     = controller.get(departureId)(fakeRequest)

        status(result) mustBe FORBIDDEN
      }
    }

    "when channel header is missing" - {

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

      "must return BadRequest" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = applicationBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(departureId)(fakeRequest.withHeaders(Headers.create()))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }

    "when channel header contains invalid value" - {

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

      "must return BadRequest" in {

        val departureId = arbitrary[DepartureId].sample.value

        val mockAuthConnector = mock[AuthConnector]

        when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
          .thenReturn(Future.successful(validEnrolments))

        val application = applicationBuilder
          .overrides(
            bind[AuthConnector].toInstance(mockAuthConnector),
            bind[MigrationRunner].to[FakeMigrationRunner]
          )
          .build()

        running(application) {
          val actionProvider = application.injector.instanceOf[AuthenticatedGetDepartureWithoutMessagesForReadActionProvider]

          val controller = new Harness(actionProvider)
          val result     = controller.get(departureId)(fakeRequest.withHeaders("channel" -> "web2"))

          status(result) mustEqual BAD_REQUEST
          contentAsString(result) mustEqual "Missing channel header or incorrect value specified in channel header"
        }
      }
    }
  }
}
