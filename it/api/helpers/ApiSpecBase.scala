package api.helpers

import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import repositories.MongoSuite

trait ApiSpecBase extends AnyFreeSpec with Matchers with ScalaFutures with GuiceOneServerPerSuite with IntegrationPatience with WiremockSuite with MongoSuite with StubHelper {

}
