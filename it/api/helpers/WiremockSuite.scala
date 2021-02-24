package api.helpers

import com.github.tomakehurst.wiremock.WireMockServer
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}

trait WiremockSuite extends BeforeAndAfterAll with BeforeAndAfterEach {
  this: Suite =>

  protected val server: WireMockServer = new WireMockServer(11111)

  protected def portConfigKeys: Seq[String]

  def portConfigKeysAndValues: Seq[(String, String)] = portConfigKeys.map(_ -> "11111")

  protected lazy val appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder()
      .configure(portConfigKeysAndValues: _*)
      .overrides(bindings: _*)

  protected def bindings: Seq[GuiceableModule] = Seq.empty

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