package repositories

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import reactivemongo.api.commands.Command
import reactivemongo.api.{BSONSerializationPack, FailoverStrategy, ReadPreference}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.errors.ReactiveMongoException

import scala.concurrent.ExecutionContext.Implicits.global

trait FailOnUnindexedQueries extends MongoSuite with BeforeAndAfterAll with ScalaFutures with TestSuiteMixin {
  self: TestSuite =>

  private val commandRunner = Command.run(BSONSerializationPack, FailoverStrategy())

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    database.map(_.drop()).futureValue

    commandRunner(
      db = MongoSuite.connection.flatMap(_.database("admin")).futureValue,
      command = commandRunner.rawCommand(BSONDocument("setParameter" -> 1, "notablescan" -> 1))
    ).one[BSONDocument](ReadPreference.primaryPreferred).futureValue
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    commandRunner(
      db      = MongoSuite.connection.flatMap(_.database("admin")).futureValue,
      command = commandRunner.rawCommand(BSONDocument("setParameter" -> 1, "notablescan" -> 0))
    ).one[BSONDocument](ReadPreference.primaryPreferred).futureValue
  }

  abstract override def withFixture(test: NoArgTest): Outcome =
    super.withFixture(test) match {
      case Failed(e: ReactiveMongoException) if e.getMessage() contains "No query solutions" =>
        Failed("Mongo query could not be satisfied by an index:\n" + e.getMessage())
      case thing@Failed(e) =>
        thing

      case other => other
    }
}
