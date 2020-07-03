package repositories

import models.DepartureId
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import reactivemongo.play.json.collection.JSONCollection
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter

import scala.concurrent.ExecutionContext.Implicits.global

class DepartureIdRepositorySpec extends AnyFreeSpec with Matchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience {

  private val service = app.injector.instanceOf[DepartureIdRepository]

  "DepartureIdRepository" - {

    "must generate sequential DepartureIds starting at 1 when no record exists within the database" in {

      database.flatMap(_.drop()).futureValue

      val first  = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe DepartureId(1)
      second mustBe DepartureId(2)
    }

    "must generate sequential DepartureIds when a record exists within the database" in {

      database.flatMap {
        db =>
          db.drop().flatMap {
            _ =>
              db.collection[JSONCollection](DepartureIdRepository.collectionName)
                .insert(ordered = false)
                .one(
                  Json.obj(
                    "_id"        -> "record_id",
                    "last-index" -> 1
                  ))
          }
      }.futureValue

      val first  = service.nextId().futureValue
      val second = service.nextId().futureValue

      first mustBe DepartureId(2)
      second mustBe DepartureId(3)
    }
  }

}
