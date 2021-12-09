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

package base

import controllers.actions.AuthenticateActionProvider
import controllers.actions.FakeAuthenticateActionProvider
import models.Departure
import models.MessageWithStatus
import org.scalactic.Equality
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier

import scala.xml.Utility
import scala.xml.XML
import com.kenshoo.play.metrics.Metrics
import migrations.MigrationRunner
import migrations.FakeMigrationRunner
import utils.TestMetrics

trait SpecBase extends AnyFreeSpec with Matchers with MockitoSugar with ScalaFutures with OptionValues with EitherValues {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  def fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("", "")

  protected def baseApplicationBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(
        "metrics.jvm" -> false
      )
      .overrides(
        bind[AuthenticateActionProvider].to[FakeAuthenticateActionProvider],
        bind[Metrics].toInstance(new TestMetrics),
        bind[MigrationRunner].to[FakeMigrationRunner]
      )

  implicit val messageWithStatusEquality: Equality[MessageWithStatus] = (a: MessageWithStatus, b: Any) =>
    b match {
      case x: MessageWithStatus =>
        val normalisedAMessage = Utility.trim(XML.loadString(a.message.toString))
        val normalisedXMessage = Utility.trim(XML.loadString(x.message.toString))

        val normalisedA = a.copy(message = normalisedAMessage)
        val normalisedX = a.copy(message = normalisedXMessage)

        normalisedA == normalisedX

      case _ => false
  }

  implicit val departureEquality: Equality[Departure] = (a: Departure, b: Any) =>
    b match {
      case x: Departure => {
        val unAppliedA = Departure.unapply(a).get
        val unAppliedX = Departure.unapply(x).get

        val normalisedMessagesA = unAppliedA._10.map {
          y =>
            Utility.trim(XML.loadString(y.message.toString()))
        }
        val normalisedA = unAppliedA.copy(_10 = normalisedMessagesA)

        val normalisedMessagesX = unAppliedX._10.map {
          z =>
            Utility.trim(XML.loadString(z.message.toString()))
        }
        val normalisedX = unAppliedX.copy(_10 = normalisedMessagesX)

        normalisedA == normalisedX
      }
      case _ => false
  }
}
