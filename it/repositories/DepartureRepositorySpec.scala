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

package repositories

import java.time.{Clock, Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneId, ZoneOffset}

import cats.data.NonEmptyList
import generators.ModelGenerators
import models.ChannelType.{api, web}
import models.DepartureStatus.{DepartureSubmitted, Initialized, MrnAllocated, PositiveAcknowledgement}
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models._
import models.response.ResponseDeparture
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source
import org.scalatest._
import org.scalatest.concurrent.IntegrationPatience
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers.running
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.{Format, JsonHelper}
import play.api.inject.bind

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

class DepartureRepositorySpec
    extends AnyFreeSpec
    with TryValues
    with OptionValues
    with ModelGenerators
    with Matchers
    with ScalaFutures
    with MongoSuite
    with GuiceOneAppPerSuite
    with IntegrationPatience
    with MongoDateTimeFormats
    with JsonHelper {

  private val service = app.injector.instanceOf[DepartureRepository]

  private val instant = Instant.now
  implicit private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException((_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
                                    None,
                                    implicitly[source.Position])
  }

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
    message   <- arbitrary[MessageWithStatus]
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

        whenReady(result) {
          r =>
            r.value mustBe departure
        }
      }
    }

    "get(departureId: DepartureId, channelFilter: ChannelType)" - {
      "must get an departure when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value.copy(channel = api)

        service.insert(departure).futureValue
        val result = service.get(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), channel = api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(2), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(1), web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "setMessageState" - {
      "must update the status of a specific message in an existing departure" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value

        service.insert(departure).futureValue

        service.setMessageState(departure.departureId, 0, SubmissionSucceeded).futureValue

        val updatedDeparture = service.get(departure.departureId, departure.channel)

        whenReady(updatedDeparture) {
          r =>
            typeMatchOnTestValue(r.value.messages.head) {
              result: MessageWithStatus =>
                result.status mustEqual SubmissionSucceeded
            }
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.setMessageState(DepartureId(2), 0, SubmissionSucceeded)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }

      "must fail if the message doesn't exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.setMessageState(DepartureId(1), 5, SubmissionSucceeded)

        whenReady(result) {
          r =>
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

        whenReady(result) {
          r =>
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

        val departureDeclarationMessage =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DepartureDeclaration, messageBody, departure.nextMessageCorrelationId, convertXmlToJson(messageBody.toString))

        service.insert(departure).futureValue
        service.addNewMessage(departure.departureId, departureDeclarationMessage).futureValue.success

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }

        whenReady(result) {
          r =>
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

        val departure = arbitrary[Departure].sample.value copy(status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

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

        val departureDeclaration =
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DepartureDeclaration, messageBody, messageCorrelationId = 1, convertXmlToJson(messageBody.toString))

        service.insert(departure).futureValue
        val result = service.addNewMessage(DepartureId(2), departureDeclaration)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }
    }

    "updateDeparture" - {
      "must update the departure and return a Success Unit when successful" in {
        database.flatMap(_.drop()).futureValue

        val departureStatus = DepartureStatusUpdate(Initialized)
        val departure       = departureWithOneMessage.sample.value.copy(status = PositiveAcknowledgement)
        val selector        = DepartureIdSelector(departure.departureId)

        service.insert(departure).futureValue

        service.updateDeparture(selector, departureStatus).futureValue

        val updatedDeparture = service.get(departure.departureId, departure.channel).futureValue.value

        updatedDeparture.status mustEqual departureStatus.departureStatus
      }

      "must return a Failure if the selector does not match any documents" in {
        database.flatMap(_.drop()).futureValue

        val departureStatus = DepartureStatusUpdate(Initialized)
        val departure       = departureWithOneMessage.sample.value copy (departureId = DepartureId(1), status = MrnAllocated)
        val selector        = DepartureIdSelector(DepartureId(2))

        service.insert(departure).futureValue

        val result = service.updateDeparture(selector, departureStatus).futureValue

        val updatedDeparture = service.get(departure.departureId, departure.channel).futureValue.value

        result mustBe a[Failure[_]]
        updatedDeparture.status must not be (departureStatus.departureStatus)
      }
    }

    "setDepartureStateAndMessageState" - {
      "must update the status of the departure and the message in an departure" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(status = DepartureStatus.Initialized)
        val messageId = MessageId.fromIndex(0)

        service.insert(departure).futureValue
        service.setDepartureStateAndMessageState(departure.departureId, messageId, DepartureSubmitted, SubmissionSucceeded).futureValue

        val updatedDeparture = service.get(departure.departureId, departure.channel)

        whenReady(updatedDeparture) {
          r =>
            r.value.status mustEqual DepartureSubmitted

            typeMatchOnTestValue(r.value.messages.head) {
              result: MessageWithStatus =>
                result.status mustEqual SubmissionSucceeded
            }
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val departure = departureWithOneMessage.sample.value.copy(departureId = DepartureId(1), status = Initialized)
        val messageId = MessageId.fromIndex(0)

        service.insert(departure).futureValue

        val setResult = service.setDepartureStateAndMessageState(DepartureId(2), messageId, DepartureSubmitted, SubmissionSucceeded)
        setResult.futureValue must not be (defined)

        val result = service.get(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value.status mustEqual Initialized
            typeMatchOnTestValue(r.value.messages.head) {
              result: MessageWithStatus =>
                result.status mustEqual SubmissionPending
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
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DeclarationRejected, messageBody, departure.nextMessageCorrelationId, convertXmlToJson(messageBody.toString))
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

        val departure = arbitrary[Departure].sample.value copy(status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

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
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.DeclarationRejected, messageBody, messageCorrelationId = 1, convertXmlToJson(messageBody.toString))
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

        val mrn        = "mrn"
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
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.MrnAllocated, messageBody, departure.nextMessageCorrelationId, convertXmlToJson(messageBody.toString))
        val newState = DepartureStatus.MrnAllocated

        service.insert(departure).futureValue
        val addMessageResult =
          service.setMrnAndAddResponseMessage(departure.departureId, mrnAllocatedMessage, newState, MovementReferenceNumber(mrn)).futureValue

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

        val departure = arbitrary[Departure].sample.value copy(status = DepartureStatus.DepartureSubmitted, departureId = DepartureId(1))

        val mrn        = "mrn"
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
          MessageWithoutStatus(LocalDateTime.of(dateOfPrep, timeOfPrep), MessageType.MrnAllocated, messageBody, departure.nextMessageCorrelationId, convertXmlToJson(messageBody.toString))
        val newState = DepartureStatus.DepartureRejected

        service.insert(departure).futureValue
        val addMessageResult = service.setMrnAndAddResponseMessage(DepartureId(2), mrnAllocatedMessage, newState, MovementReferenceNumber(mrn)).futureValue

        addMessageResult mustBe a[Failure[_]]
      }
    }

    "fetchAllDepartures" - {

      def convertToDepartureWithoutMessages(departure: Departure): DepartureWithoutMessages =
        DepartureWithoutMessages(departure.departureId, departure.channel, departure.eoriNumber, departure.movementReferenceNumber, departure.referenceNumber, departure.status, departure.created, departure.updated)


      "return DeparturesWithoutMessages that match an eoriNumber and channel type" in {
        database.flatMap(_.drop()).futureValue

        val app = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val eoriNumber: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val departure3 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = web)

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[DepartureRepository]
          val departures = Seq(departure1, departure2, departure3)
          val jsonArr = departures.map(Json.toJsObject(_))
          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          repository.fetchAllDepartures(eoriNumber, api, None).futureValue mustBe Seq(convertToDepartureWithoutMessages(departure1))
          repository.fetchAllDepartures(eoriNumber, web, None).futureValue mustBe Seq(convertToDepartureWithoutMessages(departure3))
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {
        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value

        val app = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val departure1 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(channel = api)

        running(app) {
          started(app).futureValue

          val respository = app.injector.instanceOf[DepartureRepository]
          val allDepartures = Seq(departure1, departure2)
          val jsonArr = allDepartures.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result = respository.fetchAllDepartures(eoriNumber, api, None).futureValue

          result mustBe Seq.empty[DepartureWithoutMessages]
        }
      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(stubClock))
          .configure("metrics.jvm" -> false)
          .build()

        val departure1 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = api, updated = LocalDateTime.of(2021, 4, 30, 9, 30, 31))
        val departure2 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = api, updated = LocalDateTime.of(2021, 4, 30, 9, 35, 32))
        val departure3 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = api, updated = LocalDateTime.of(2021, 4, 30, 9, 30, 21))
        val departure4 = arbitrary[Departure].sample.value.copy(eoriNumber = eoriNumber, channel = api, updated = LocalDateTime.of(2021, 4, 30, 10, 15, 16))

        running(app) {

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val allMovements = Seq(departure1, departure2, departure3, departure4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val dateTime = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
          val actual = service.fetchAllDepartures(eoriNumber, api, Some(dateTime)).futureValue.toSet
          val expected = Set(departure2, departure4).map(DepartureWithoutMessages.fromDeparture)

          actual mustEqual expected
        }
      }
    }
  }
}
