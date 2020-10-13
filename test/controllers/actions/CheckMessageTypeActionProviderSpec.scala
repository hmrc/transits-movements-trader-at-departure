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
import models.MessageType._
import models._
import models.request.DepartureRequest
import models.request.DepartureResponseRequest
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.mvc.AnyContentAsEmpty
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckMessageTypeActionProviderSpec
    extends AnyFreeSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with OptionValues
    with ScalaFutures
    with EitherValues {
  val fakeDeparture: Departure = arbitraryDeparture.arbitrary.sample.value

  val responseMessages = Map(
    PositiveAcknowledgement.code     -> PositiveAcknowledgementResponse,
    MrnAllocated.code                -> MrnAllocatedResponse,
    DeclarationRejected.code         -> DepartureRejectedResponse,
    ControlDecisionNotification.code -> ControlDecisionNotificationResponse,
    NoReleaseForTransit.code         -> NoReleaseForTransitResponse,
    ReleaseForTransit.code           -> ReleaseForTransitResponse,
    CancellationDecision.code        -> CancellationDecisionResponse,
    WriteOffNotification.code        -> WriteOffNotificationResponse,
    GuaranteeNotValid.code           -> GuaranteeNotValidResponse
  )

  class Harness extends CheckMessageTypeAction() {
    def run[A](request: DepartureRequest[A]): Future[Either[Result, DepartureResponseRequest[A]]] = refine(request)
  }

  "CheckMessageTypeAction" - {
    "will process the action when the X-Message-Type is present" in {
      forAll(Gen.oneOf(responseMessages.toSeq)) {
        case (code, response: MessageResponse) => {
          def fakeRequest =
            DepartureRequest(FakeRequest("", "").withHeaders(
                               "X-Message-Type" -> code
                             ),
                             fakeDeparture)

          val action: Harness = new Harness()

          val result = action.run(fakeRequest)

          whenReady(result) {
            r: Either[Result, DepartureResponseRequest[AnyContentAsEmpty.type]] =>
              r.isRight mustBe true
              r.right.get.messageResponse mustBe response
          }
        }
      }
    }
    "must return an BadRequest when the X-Message-Type is missing" in {
      def fakeRequest = DepartureRequest(FakeRequest("", ""), fakeDeparture)

      val harness = new Harness

      val result = harness.run(fakeRequest)

      whenReady(result) {
        r =>
          r.isLeft mustBe true
          status(Future.successful(r.left.value)) mustEqual BAD_REQUEST
      }
    }

    "must return an BadRequest when the X-Message-Type is invalid" in {
      def fakeRequest =
        DepartureRequest(FakeRequest("", "").withHeaders(
                           "X-Message-Type" -> "Invalid-type"
                         ),
                         fakeDeparture)

      val harness = new Harness

      val result = harness.run(fakeRequest)

      whenReady(result) {
        r =>
          r.isLeft mustBe true
          status(Future.successful(r.left.value)) mustEqual BAD_REQUEST
      }
    }
  }
}
