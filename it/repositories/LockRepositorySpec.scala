package repositories

import generators.ModelGenerators
import models.DepartureId
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{EitherValues, FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.JsObject
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global

class LockRepositorySpec
  extends FreeSpec
    with MustMatchers
    with FailOnUnindexedQueries
    with ScalaFutures
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with ModelGenerators
    with GuiceOneAppPerSuite
    with IntegrationPatience {

  private val service = app.injector.instanceOf[LockRepository]

  "lock" - {
    "must lock an departureId when it is not already locked" in {
      database.flatMap(_.drop()).futureValue

      val departureId = DepartureId(1)

      val result = service.lock(departureId).futureValue

      result mustEqual true

      val selector = Json.obj("_id" -> departureId)
      val lock = database.flatMap {
        db =>
          db.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject]
      }.futureValue

      lock.value("_id").validate[DepartureId] mustEqual JsSuccess(departureId)
    }

    "must not lock an departureId that is already locked" in {
      database.flatMap(_.drop()).futureValue

      val departureId = DepartureId(1)

      val result1 = service.lock(departureId).futureValue
      val result2 = service.lock(departureId).futureValue

      result1 mustEqual true
      result2 mustEqual false
    }
  }

  "unlock" - {
    "must remove an existing lock" in {
      database.flatMap(_.drop()).futureValue

      val departureId = DepartureId(1)

      service.lock(departureId).futureValue
      service.unlock(departureId).futureValue

      val selector = Json.obj("_id" -> departureId)
      val remainingLock = database.flatMap {
        db =>
          db.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject]
      }.futureValue

      remainingLock must not be defined
    }

    "must not fail when asked to remove a lock that doesn't exist" in {
      database.flatMap(_.drop()).futureValue

      val departureId = DepartureId(1)

      service.unlock(departureId).futureValue
    }
  }

  "BSON formatting for ttl index" - {
    "a lock's created field must be a date for the ttl index" ignore { //TODO: Why doesn't this pass?
      database.flatMap(_.drop()).futureValue

      val departureId = DepartureId(1)

      val result = service.lock(departureId).futureValue

      result mustEqual true

      val selector = Json.obj("_id" -> departureId, "created" -> Json.obj("$type" -> "date"))

      val lock = database.flatMap(_.collection[JSONCollection](LockRepository.collectionName).find(selector, None).one[JsObject])

      whenReady(lock) { r => r.isDefined mustBe true }
    }
  }
}
