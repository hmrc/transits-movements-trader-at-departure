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
import models.ChannelType.web
import models.Departure
import models.DepartureId
import org.mockito.ArgumentMatchers
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
import play.api.mvc.Headers
import play.api.mvc.Results
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.DepartureRepository
import repositories.LockRepository

import scala.concurrent.Future

class GetDepartureForWriteActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with MockitoSugar
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues {

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  class Harness(getAndLock: GetDepartureForWriteActionProvider) {

    def get(departureId: DepartureId): Action[AnyContent] = getAndLock(departureId) {
      request =>
        Results.Ok(request.departure.departureId.toString)
    }

    def failingAction(departureId: DepartureId): Action[AnyContent] = getAndLock(departureId).async {
      _ =>
        Future.failed(new Exception())
    }
  }

  "get departure for edit" - {

    "must lock a departure, retrieve it, then unlock it when the departure exists" in {

      val departure = arbitrary[Departure].sample.value

      val mockDepartureRepository = mock[DepartureRepository]
      val mockLockRepository      = mock[LockRepository]

      when(mockDepartureRepository.get(any())) thenReturn Future.successful(Some(departure))
      when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
      when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetDepartureForWriteActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(departure.departureId)(fakeRequest.withHeaders("channel" -> departure.channel.toString))

      status(result) mustEqual OK
      contentAsString(result) mustEqual departure.departureId.toString
      verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
      verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
    }

    "must lock an departure, unlock it, and return Not Found when the departure cannot be found" in {

      val departure = arbitrary[Departure].sample.value

      val mockDepartureRepository = mock[DepartureRepository]
      val mockLockRepository      = mock[LockRepository]

      when(mockDepartureRepository.get(any())) thenReturn Future.successful(None)
      when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
      when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetDepartureForWriteActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(departure.departureId)(fakeRequest.withHeaders("channel" -> web.toString))

      status(result) mustEqual NOT_FOUND
      verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
      verify(mockLockRepository, times(1)).unlock(eqTo(departure.departureId))
    }

    "must return Locked if a lock cannot be acquired" in {

      val departure = arbitrary[Departure].sample.value

      val mockDepartureRepository = mock[DepartureRepository]
      val mockLockRepository      = mock[LockRepository]

      when(mockLockRepository.lock(any())) thenReturn Future.successful(false)

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetDepartureForWriteActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.get(departure.departureId)(fakeRequest)

      status(result) mustEqual LOCKED
      verify(mockLockRepository, never).unlock(any())
      verify(mockDepartureRepository, never).get(any(), any())
    }

    "must unlock a departure and return Internal Server Error if the main action fails" in {

      val departure = arbitrary[Departure].sample.value

      val mockDepartureRepository = mock[DepartureRepository]
      val mockLockRepository      = mock[LockRepository]

      when(mockDepartureRepository.get(any())) thenReturn Future.successful(Some(departure))
      when(mockLockRepository.lock(any())) thenReturn Future.successful(true)
      when(mockLockRepository.unlock(any())) thenReturn Future.successful(())

      val application = new GuiceApplicationBuilder()
        .overrides(
          bind[DepartureRepository].toInstance(mockDepartureRepository),
          bind[LockRepository].toInstance(mockLockRepository)
        )

      val actionProvider = application.injector.instanceOf[GetDepartureForWriteActionProvider]

      val controller = new Harness(actionProvider)
      val result     = controller.failingAction(departure.departureId)(fakeRequest.withHeaders("channel" -> departure.channel.toString))

      status(result) mustEqual INTERNAL_SERVER_ERROR
      verify(mockLockRepository, times(1)).lock(eqTo(departure.departureId))
    }
  }
}
