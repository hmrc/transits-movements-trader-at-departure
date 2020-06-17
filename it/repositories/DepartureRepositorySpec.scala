package repositories

import generators.ModelGenerators
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{EitherValues, FreeSpec, MustMatchers, OptionValues, TryValues}
import org.scalatest.concurrent.IntegrationPatience
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers.running
import reactivemongo.play.json.collection.JSONCollection
import uk.gov.hmrc.transitsmovementstraderatdeparture.models.{Departure, MongoDateTimeFormats}
import uk.gov.hmrc.transitsmovementstraderatdeparture.repositories.DepartureRepository

import scala.concurrent.ExecutionContext.Implicits.global

class DepartureRepositorySpec extends FreeSpec
  with MustMatchers
  with FailOnUnindexedQueries
  with IntegrationPatience
  with OptionValues
  with EitherValues
  with TryValues
  with ModelGenerators
  with MongoDateTimeFormats {

  "DepartureRepository" - {

    def builder = new GuiceApplicationBuilder()

    "insert" must {

      "must persist Departure within mongoDB" in {
        database.flatMap(_.drop()).futureValue

        val app: Application = builder.build()

        val departure = arbitrary[Departure].sample.value

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[DepartureRepository]

          repository.insert(departure).futureValue

//          val selector = Json.obj("eoriNumber" -> arrival.eoriNumber)

          val result = database.flatMap {
            result =>
              result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
          }.futureValue

          result.value mustBe departure
        }
      }
    }

  }

}
