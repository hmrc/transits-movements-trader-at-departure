package repositories

import java.time.{LocalDate, LocalDateTime, LocalTime}

import cats.data.NonEmptyList
import generators.ModelGenerators
import models.DepartureStatus.{DepartureSubmitted, Initialized}
import models.MessageStatus.{SubmissionPending, SubmissionSucceeded}
import models.{Departure, DepartureId, DepartureStatus, MessageId, MessageType, MessageWithStatus, MessageWithoutStatus, MongoDateTimeFormats, MovementReferenceNumber}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.Format

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
class DepartureRepositorySpec extends AnyFreeSpec with TryValues with OptionValues with ModelGenerators with Matchers with ScalaFutures with MongoSuite with GuiceOneAppPerSuite with IntegrationPatience  with MongoDateTimeFormats {

  private val service = app.injector.instanceOf[DepartureRepository]

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult => throw new TestFailedException((_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"), None, implicitly[source.Position])
  }

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
    message <- arbitrary[MessageWithStatus]
  } yield departure.copy(messages = NonEmptyList.one(message.copy(status = SubmissionPending)))


  "DepartureRepository" - {

    "insert" - {
      "must persist Departure within mongoDB" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        service.insert(departure).futureValue

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }

        whenReady(result) { r =>
          r.value mustBe departure
        }
      }
    }

    "get(departureId: DepartureId)" - {
      "must get an departure when it exists" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        service.insert(departure).futureValue
        val result = service.get(departure.departureId)

        whenReady(result) { r =>
          r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.get(DepartureId(2))

        whenReady(result) { r =>
          r.isDefined mustBe false
        }
      }
    }

    "get(eoriNumber: String, reference: String)" - {
      "must get an departure if one exists with a matching eoriNumber and referenceNumber" in {
        database.flatMap(_.drop()).futureValue

            val referenceNumber = "apples"
            val eori            = "eori"
            val departure       = arbitrary[Departure].sample.value copy (eoriNumber = eori, referenceNumber = referenceNumber)
            service.insert(departure).futureValue
            val result = service.get(eori, referenceNumber)
            whenReady(result) { r =>
              r.value mustEqual departure
            }
      }

      "must return a None if any exist with a matching eoriNumber but no matching referenceNumber" in {
        database.flatMap(_.drop()).futureValue

        val referenceNumber      = arbitrary[String].sample.value
        val otherReferenceNumber = arbitrary[String].sample.value

        val eori    = "eori"
        val departure = arbitrary[Departure].sample.value copy (eoriNumber = eori, referenceNumber = otherReferenceNumber)

        service.insert(departure).futureValue

        val result = service.get(eori, referenceNumber)

        whenReady(result) {
          r => r mustEqual None
        }
      }

      "must return a None if any exist with a matching referenceNumber but no matching eoriNumber" in {
        database.flatMap(_.drop()).futureValue

        val referenceNumber      = arbitrary[String].sample.value

        val eori      = "eori"
        val otherEori = "otherEori"
        val departure = arbitrary[Departure].sample.value copy (eoriNumber = otherEori, referenceNumber = referenceNumber)

        service.insert(departure).futureValue

        val result = service.get(eori, referenceNumber)

        whenReady(result) { r =>
          r mustEqual None
        }
      }

      "must return a None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val referenceNumber      = arbitrary[String].sample.value
        val otherReferenceNumber = arbitrary[String].sample.value

        val eori      = "eori"
        val otherEori = "otherEori"
        val departure = arbitrary[Departure].sample.value copy (eoriNumber = otherEori, referenceNumber = otherReferenceNumber)

        service.insert(departure).futureValue

        val result = service.get(eori, referenceNumber)

        whenReady(result) { r =>
          r mustEqual None
        }
      }
    }

    "setMessageState" - {
      "must update the status of a specific message in an existing departure" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value

        service.insert(departure).futureValue

        service.setMessageState(departure.departureId, 0, SubmissionSucceeded).futureValue

        val updatedDeparture = service.get(departure.departureId)

        whenReady(updatedDeparture) { r =>
          typeMatchOnTestValue(r.value.messages.head) {
            result: MessageWithStatus => result.status mustEqual SubmissionSucceeded
          }
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(departureId = DepartureId(1))

          service.insert(departure).futureValue
          val result = service.setMessageState(DepartureId(2), 0, SubmissionSucceeded)

          whenReady(result) { r =>
            r mustBe a[Failure[_]]
          }
      }

      "must fail if the message doesn't exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value copy(departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.setMessageState(DepartureId(1), 5, SubmissionSucceeded)

        whenReady(result) { r =>
          r mustBe a[Failure[_]]
        }
      }

      "must fail if the message does not have a status" in {
        database.flatMap(_.drop()).futureValue

        val preGenDeparture = departureWithOneMessage.sample.value
        val departure = preGenDeparture.copy(departureId = DepartureId(1),
        messages = NonEmptyList.one(arbitrary[MessageWithoutStatus].sample.value))

        service.insert(departure).futureValue
        val result = service.setMessageState(DepartureId(1), 0, SubmissionSucceeded)

        whenReady(result) { r =>
          r mustBe a[Failure[_]]
        }
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value.copy(status = DepartureStatus.DepartureSubmitted)

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC015B>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <RefNumHEA4>abc</RefNumHEA4>
            </HEAHEA>
          </CC015B>

        val departureDeclarationMessage = MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DepartureDeclaration, messageBody, departure.nextMessageCorrelationId)

        service.insert(departure).futureValue
        service.addNewMessage(departure.departureId, departureDeclarationMessage).futureValue.success

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }

        whenReady(result) { r =>
          val updatedDeparture = r.value

          updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 1
          updatedDeparture.updated mustEqual departureDeclarationMessage.dateTime
          updatedDeparture.status mustEqual departure.status
          updatedDeparture.messages.size - departure.messages.size mustEqual 1
          updatedDeparture.messages.last mustEqual departureDeclarationMessage
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC015B>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <RefNumHEA4>abc</RefNumHEA4>
            </HEAHEA>
          </CC015B>

        val departureDeclaration = MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DepartureDeclaration, messageBody, messageCorrelationId = 1)

        service.insert(departure).futureValue
        val result = service.addNewMessage(DepartureId(2), departureDeclaration)

        whenReady(result) { r =>
          r mustBe a[Failure[_]]
        }
      }
    }

    "setDepartureStateAndMessageState" - {
      "must update the status of the departure and the message in an departure" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(status = DepartureStatus.Initialized)
        val messageId = MessageId.fromIndex(0)

        service.insert(departure).futureValue
        service.setDepartureStateAndMessageState(departure.departureId, messageId.index, DepartureSubmitted, SubmissionSucceeded).futureValue

        val updatedDeparture = service.get(departure.departureId)

        whenReady(updatedDeparture) { r =>
          r.value.status mustEqual DepartureSubmitted

          typeMatchOnTestValue(r.value.messages.head) {
            result: MessageWithStatus => result.status mustEqual SubmissionSucceeded
          }
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(departureId = DepartureId(1), status = Initialized)
        val messageId =  MessageId.fromIndex(0)

        service.insert(departure).futureValue

        val setResult = service.setDepartureStateAndMessageState(DepartureId(2), messageId.index, DepartureSubmitted, SubmissionSucceeded)
        setResult.futureValue must not be(defined)

        val result = service.get(departure.departureId)

        whenReady(result) { r =>
          r.value.status mustEqual Initialized
          typeMatchOnTestValue(r.value.messages.head) {
            result: MessageWithStatus => result.status mustEqual SubmissionPending
          }
        }
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC016A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC016A>

        val declarationRejectedMessage =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DeclarationRejected, messageBody, departure.nextMessageCorrelationId)
        val newState = DepartureStatus.DepartureRejected

        service.insert(departure).futureValue
        val addMessageResult = service.addResponseMessage(departure.departureId, declarationRejectedMessage, newState).futureValue

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }.futureValue

        val updatedDeparture = result.value

        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.updated mustEqual declarationRejectedMessage.dateTime
        updatedDeparture.status mustEqual newState
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual declarationRejectedMessage
    }
      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC025A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>MRN</DocNumHEA5>
            </HEAHEA>
          </CC025A>

        val declarationRejected =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DeclarationRejected, messageBody, messageCorrelationId = 1)
        val newState = DepartureStatus.DepartureRejected

        service.insert(departure).futureValue
        val result = service.addResponseMessage(DepartureId(2), declarationRejected, newState).futureValue

        result mustBe a[Failure[_]]
      }
  }

    "setMrnAndAddResponseMessage" - {
      "must add a message, update the status of a document, update the timestamp, and update the MRN" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        val mrn = "mrn"
        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC028A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn}</DocNumHEA5>
            </HEAHEA>
          </CC028A>

        val mrnAllocatedMessage =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.MrnAllocated, messageBody, departure.nextMessageCorrelationId)
        val newState = DepartureStatus.MrnAllocated

        service.insert(departure).futureValue
        val addMessageResult = service.setMrnAndAddResponseMessage(departure.departureId, mrnAllocatedMessage, newState, MovementReferenceNumber(mrn)).futureValue

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }.futureValue

        val updatedDeparture = result.value

        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.updated mustEqual mrnAllocatedMessage.dateTime
        updatedDeparture.status mustEqual newState
        updatedDeparture.movementReferenceNumber mustEqual Some(MovementReferenceNumber(mrn))
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual mrnAllocatedMessage
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

        val mrn = "mrn"
        val dateOfPrep = LocalDate.now()
        val timeOfPrep = LocalTime.of(1, 1)
        val messageBody =
          <CC028A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn}</DocNumHEA5>
            </HEAHEA>
          </CC028A>

        val mrnAllocatedMessage =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.MrnAllocated, messageBody, departure.nextMessageCorrelationId)
        val newState = DepartureStatus.DepartureRejected

        service.insert(departure).futureValue
        val addMessageResult = service.setMrnAndAddResponseMessage(DepartureId(2), mrnAllocatedMessage, newState, MovementReferenceNumber(mrn)).futureValue

        addMessageResult mustBe a[Failure[_]]
      }

    }
  }


  }
