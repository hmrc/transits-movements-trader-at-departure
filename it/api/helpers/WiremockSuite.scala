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

package api.helpers

import com.github.tomakehurst.wiremock.WireMockServer
import com.kenshoo.play.metrics.Metrics
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import utils.TestMetrics

trait WiremockSuite extends BeforeAndAfterAll with BeforeAndAfterEach {
  this: Suite =>

  protected val server: WireMockServer = new WireMockServer(11111)

  protected def portConfigKeys: Seq[String]

  def portConfigKeysAndValues: Seq[(String, String)] = portConfigKeys.map(_ -> "11111")

  protected lazy val appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(portConfigKeysAndValues: _*)
      .overrides(bindings: _*)

  protected def bindings: Seq[GuiceableModule] = Seq(
    bind[Metrics].toInstance(new TestMetrics)
  )

  override def beforeAll(): Unit = {
    server.start()
    super.beforeAll()
  }

  override def beforeEach(): Unit = {
    server.resetAll()
    super.beforeEach()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    server.stop()
  }
}