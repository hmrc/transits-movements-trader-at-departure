/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.data.Chain
import cats.data.Ior
import cats.data.NonEmptyList
import config.AppConfig
import generators.ModelGenerators
import models.ChannelType.Api
import models.ChannelType.Web
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models._
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest._
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers.running
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import utils.Format
import utils.JsonHelper

import java.time._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class DepartureRepositorySpec
    extends AnyFreeSpec
    with TryValues
    with OptionValues
    with ModelGenerators
    with Matchers
    with MongoSuite
    with GuiceOneAppPerSuite
    with MongoDateTimeFormats
    with JsonHelper {

  implicit override lazy val app: Application = GuiceApplicationBuilder()
    .configure("feature-flags.testOnly.enabled" -> true)
    .build()

  private val service   = app.injector.instanceOf[DepartureRepository]
  private val appConfig = app.injector.instanceOf[AppConfig]
  val localDate         = LocalDate.now()
  val localTime         = LocalTime.of(1, 1)
  val localDateTime     = LocalDateTime.of(localDate, localTime)
  implicit val clock    = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)

  def typeMatchOnTestValue[A, B](testValue: A)(test: B => Unit)(implicit bClassTag: ClassTag[B]) = testValue match {
    case result: B => test(result)
    case failedResult =>
      throw new TestFailedException(
        (_: StackDepthException) => Some(s"Test for ${bClassTag.runtimeClass}, but got a ${failedResult.getClass}"),
        None,
        implicitly[Position]
      )
  }

  def nonEmptyListOfSize[T](size: Int)(f: (T, Int) => T)(implicit a: Arbitrary[T]): Gen[NonEmptyList[T]] =
    Gen.listOfN(size, arbitrary[T])
      // Don't generate duplicate IDs
      .map(_.foldLeft((Chain.empty[T], 1)) {
        case ((ts, id), t) =>
          (ts :+ f(t, id), id + 1)
      }).map {
      case (ts, _) =>
        NonEmptyList.fromListUnsafe(ts.toList)
    }

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
    message   <- arbitrary[MessageWithStatus]
  } yield departure.copy(messages = NonEmptyList.one(message.copy(status = SubmissionPending)))

  private def departureWithoutMessages(departure: Departure): DepartureWithoutMessages = {
    DepartureWithoutMessages(
      departure.departureId,
      departure.channel,
      departure.eoriNumber,
      departure.movementReferenceNumber,
      departure.referenceNumber,
      departure.created,
      departure.lastUpdated,
      departure.notificationBox,
      departure.nextMessageId,
      departure.nextMessageCorrelationId,
      departure.messages.map(x => MessageMetaData(x.messageType, x.dateTime)).toList
    )
  }

  private val departureId1 = arbitrary[DepartureId].sample.value
  private val departureId2 = departureId1.copy(index = departureId1.index + 1)
  private val departureId3 = departureId2.copy(index = departureId2.index + 1)
  private val departureId4 = departureId3.copy(index = departureId3.index + 1)


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

    "getMaxDepartureId" - {
      "must return the highest departure id in the database" in {
        database.flatMap(_.drop()).futureValue

        val departures = List.tabulate(5)(
          index => arbitrary[Departure].sample.value.copy(departureId = DepartureId(index + 1))
        )

        service.bulkInsert(departures).futureValue

        service.getMaxDepartureId.futureValue.value mustBe DepartureId(5)
      }
    }

    "get(departureId: DepartureId)" - {
      "must get an departure when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        service.insert(departure).futureValue
        val result = service.get(departure.departureId)

        whenReady(result) {
          r =>
            r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.get(DepartureId(2))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }


    "getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType])" - {
        
      val mType = MessageType.ReleaseForTransit

      val node = NodeSeq.fromSeq(Seq(<CC029B>m</CC029B>))

      val message = MessageWithStatus(
          MessageId(1),
          LocalDateTime.now(),
          MessageType.ReleaseForTransit,
          node,
          MessageStatus.SubmissionSucceeded,
          1
        )

      val otherMessage = MessageWithStatus(
          MessageId(2),
          LocalDateTime.now(),
          MessageType.NoReleaseForTransit,
          node,
          MessageStatus.SubmissionSucceeded,
          2
        )

      "must get the appropriate messages when they exist and has the right channel type" in {
        database.flatMap(_.drop()).futureValue
        val departure = arbitrary[Departure].sample.value copy (messages = NonEmptyList[Message](message, List.empty))

        service.insert(departure).futureValue

        // We copy the message node because the returned node isn't equal, even though it's 
        // identical for our purposes. As it's not what we are really testing, we just copy the
        // original message across so it doesn't fail the equality check
        val result = service.getMessagesOfType(departure.departureId, departure.channel, List(mType))
          .map(opt => opt.map(ar => ar.messages.asInstanceOf[List[MessageWithStatus]].map(x => x copy (message = node))))

        whenReady(result) {
          r =>  
            r mustBe defined
            r.get must contain theSameElementsAs List(message)
        }
      }

      "must only return the appropriate messages when an arrival is matched" in {
        database.flatMap(_.drop()).futureValue
        val departure = arbitrary[Departure].sample.value copy (messages = NonEmptyList[Message](message, List(otherMessage)))

        service.insert(departure).futureValue

        // As in the previous test.
        val result = service.getMessagesOfType(departure.departureId, departure.channel, List(mType))
          .map(opt => opt.map(ar => ar.messages.asInstanceOf[List[MessageWithStatus]].map(x => x copy (message = node))))

        whenReady(result) {
          r =>
            r mustBe defined
            r.get must contain theSameElementsAs List(message)
        }
      }

      "must return an empty list when an arrival exists but no messages match" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), messages = NonEmptyList[Message](otherMessage, List.empty))

        service.insert(departure).futureValue
        val result = service.getMessagesOfType(DepartureId(1), departure.channel, List(mType))

        whenReady(result) {
          r => 
            r mustBe defined
            r.get.messages mustEqual List()
        }
      }

      "must return None when an arrival does not exist" in {
        database.flatMap(_.drop()).futureValue
        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.getMessagesOfType(DepartureId(2), departure.channel, List(mType))

        whenReady(result) {
          r => 
            r must not be defined
        }
      }

      "must return an empty list when an arrival exists but without any of the message type" in {
        database.flatMap(_.drop()).futureValue
        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.getMessagesOfType(DepartureId(2), departure.channel, List(mType))

        whenReady(result) {
          r =>
            r mustBe empty
        } 
      }

      "must return None when an arrival does exist but with the wrong channel type" in {
        database.flatMap(_.drop()).futureValue
        val departure = arbitrary[Departure].sample.value copy (channel = ChannelType.Api)

        service.insert(departure).futureValue
        val result = service.getMessagesOfType(departure.departureId, ChannelType.Web, List(mType))

        whenReady(result) {
          r => 
            r must not be defined
        }
      }

    }

    "get(departureId: DepartureId, channelFilter: ChannelType)" - {
      "must get an departure when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value.copy(channel = Api)

        service.insert(departure).futureValue
        val result = service.get(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), channel = Api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(1), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "getWithoutMessages(departureId: DepartureId)" - {
      "must get an departure when it exists" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        service.insert(departure).futureValue
        val result = service.getWithoutMessages(departure.departureId)

        whenReady(result) {
          r =>
            r.value mustEqual departureWithoutMessages(departure)
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        service.insert(departure).futureValue
        val result = service.getWithoutMessages(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(1), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "getWithoutMessages(departureId: DepartureId, channelFilter: ChannelType)" - {
      "must get an departure when it exists and has the right channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure                = arbitrary[Departure].sample.value.copy(channel = Api)

        service.insert(departure).futureValue
        val result = service.getWithoutMessages(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value mustEqual departureWithoutMessages(departure)
        }
      }

      "must return None when an departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), channel = Api)

        service.insert(departure).futureValue
        val result = service.getWithoutMessages(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        service.insert(departure).futureValue
        val result = service.get(DepartureId(1), Web)

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
        val departure       = preGenDeparture.copy(departureId = DepartureId(1), messages = NonEmptyList.one(arbitrary[MessageWithoutStatus].sample.value))

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

        val declarationMessages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            MessageType.DepartureDeclaration,
            <CC015></CC015>,
            MessageStatus.SubmissionPending,
            1
          )
        )
        val departure = arbitrary[Departure].sample.value.copy(messages = declarationMessages)

        val dateOfPrep = LocalDate.now(clock)
        val timeOfPrep = LocalTime.of(1, 1)
        val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)
        val messageBody =
          <CC015B>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <RefNumHEA4>abc</RefNumHEA4>
            </HEAHEA>
          </CC015B>

        val departureDeclarationMessage =
          MessageWithoutStatus(
            departure.nextMessageId,
            dateTime,
            MessageType.DepartureDeclaration,
            messageBody,
            departure.nextMessageCorrelationId
          )

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
            updatedDeparture.status mustEqual departure.status
            updatedDeparture.messages.size - departure.messages.size mustEqual 1
            updatedDeparture.messages.last mustEqual departureDeclarationMessage
        }
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            MessageType.DepartureDeclaration,
            <CC015></CC015>,
            MessageStatus.SubmissionPending,
            1
          )
        )
        val departure = arbitrary[Departure].sample.value copy (messages = messages, departureId = DepartureId(1))

        val dateOfPrep = LocalDate.now(clock)
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
          MessageWithoutStatus(
            departure.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            MessageType.DepartureDeclaration,
            messageBody,
            messageCorrelationId = 1
          )

        service.insert(departure).futureValue
        val result = service.addNewMessage(DepartureId(2), departureDeclaration)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        val dateOfPrep = LocalDate.now(clock)
        val timeOfPrep = LocalTime.of(1, 1)
        val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)
        val messageBody =
          <CC016A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
          </CC016A>

        val declarationRejectedMessage =
          MessageWithoutStatus(
            departure.nextMessageId,
            dateTime,
            MessageType.DeclarationRejected,
            messageBody,
            departure.nextMessageCorrelationId
          )

        service.insert(departure).futureValue
        val addMessageResult = service.addResponseMessage(departure.departureId, declarationRejectedMessage).futureValue

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }.futureValue

        val updatedDeparture = result.value

        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.status mustEqual DepartureStatus.DepartureRejected
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual declarationRejectedMessage
      }
      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            MessageType.DepartureDeclaration,
            <CC015></CC015>,
            MessageStatus.SubmissionPending,
            1
          )
        )

        val departure = arbitrary[Departure].sample.value copy (messages = messages, departureId = DepartureId(1))

        val dateOfPrep = LocalDate.now(clock)
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
          MessageWithoutStatus(
            departure.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            MessageType.DeclarationRejected,
            messageBody,
            messageCorrelationId = 1
          )

        service.insert(departure).futureValue
        val result = service.addResponseMessage(DepartureId(2), declarationRejected).futureValue

        result mustBe a[Failure[_]]
      }
    }

    "setMrnAndAddResponseMessage" - {
      "must add a message, update the status of a document, update the timestamp, and update the MRN" in {
        database.flatMap(_.drop()).futureValue

        val departure = arbitrary[Departure].sample.value

        val mrn        = "mrn"
        val dateOfPrep = LocalDate.now(clock)
        val timeOfPrep = LocalTime.of(1, 1)
        val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)
        val messageBody =
          <CC028A>
            <DatOfPreMES9>{Format.dateFormatted(dateOfPrep)}</DatOfPreMES9>
            <TimOfPreMES10>{Format.timeFormatted(timeOfPrep)}</TimOfPreMES10>
            <HEAHEA>
              <DocNumHEA5>{mrn}</DocNumHEA5>
            </HEAHEA>
          </CC028A>

        val mrnAllocatedMessage =
          MessageWithoutStatus(
            departure.nextMessageId,
            dateTime,
            MessageType.MrnAllocated,
            messageBody,
            departure.nextMessageCorrelationId
          )

        service.insert(departure).futureValue
        val addMessageResult =
          service.setMrnAndAddResponseMessage(departure.departureId, mrnAllocatedMessage, MovementReferenceNumber(mrn)).futureValue

        val selector = Json.obj("_id" -> departure.departureId)

        val result = database.flatMap {
          result =>
            result.collection[JSONCollection](DepartureRepository.collectionName).find(selector, None).one[Departure]
        }.futureValue

        val updatedDeparture = result.value

        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.status mustEqual DepartureStatus.MrnAllocated
        updatedDeparture.movementReferenceNumber.get mustEqual MovementReferenceNumber(mrn)
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual mrnAllocatedMessage
      }

      "must fail if the departure cannot be found" in {
        database.flatMap(_.drop()).futureValue

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            MessageType.DepartureDeclaration,
            <CC015></CC015>,
            MessageStatus.SubmissionPending,
            1
          )
        )
        val departure = arbitrary[Departure].sample.value copy (messages = messages, departureId = DepartureId(1))

        val mrn        = "mrn"
        val dateOfPrep = LocalDate.now(clock)
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
          MessageWithoutStatus(
            departure.nextMessageId,
            LocalDateTime.of(dateOfPrep, timeOfPrep),
            MessageType.MrnAllocated,
            messageBody,
            departure.nextMessageCorrelationId
          )

        service.insert(departure).futureValue
        val addMessageResult = service.setMrnAndAddResponseMessage(DepartureId(2), mrnAllocatedMessage, MovementReferenceNumber(mrn)).futureValue

        addMessageResult mustBe a[Failure[_]]
      }
    }

    "fetchAllDepartures" - {

      "return DeparturesWithoutMessages that match an eoriNumber and channel type" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val eoriNumber: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId2, channel = Api)
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Web)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {

          started(app).futureValue

          val repository = app.injector.instanceOf[DepartureRepository]
          val departures = Seq(departure1, departure2, departure3)
          val jsonArr    = departures.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)), 1, 1, 1)
          repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)), 1, 1, 1)
        }
      }

      "return DeparturesWithoutMessages with eoriNumber that match legacy TURN and channel type" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val turn: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = turn, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != turn).sample.value.copy(channel = Api, departureId = departureId2)
        val departure3 = arbitrary[Departure].sample.value.copy(eoriNumber = turn, channel = Web, departureId = departureId3)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[DepartureRepository]
          val departures = Seq(departure1, departure2, departure3)

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(departures)
          }.futureValue

          repository.fetchAllDepartures(Ior.left(TURN(turn)), Api, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)), 1, 1, 1)
          repository.fetchAllDepartures(Ior.left(TURN(turn)), Web, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)), 1, 1, 1)
        }
      }

      "return DeparturesWithoutMessages with eoriNumber that match either eoriNumber or legacy TURN and channel type" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val eori: String = arbitrary[String].sample.value
        val turn: String = arbitrary[String].sample.value
        val ids: Set[String] = Set(eori, turn)

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eori, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(departure => !ids.contains(departure.eoriNumber)).sample.value.copy(departureId = departureId2, channel = Api)
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = turn, channel = Web)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue

          val repository = app.injector.instanceOf[DepartureRepository]
          val departures = Seq(departure1, departure2, departure3)

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(departures)
          }.futureValue

          repository.fetchAllDepartures(Ior.both(TURN(turn), EORINumber(eori)), Api, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)), 1, 1, 1)
          repository.fetchAllDepartures(Ior.both(TURN(turn), EORINumber(eori)), Web, None).futureValue mustBe ResponseDepartures(Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)), 1, 1, 1)
        }
      }

      "must return an empty sequence when there are no movements with the same eori" in {
        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value

        val app        = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val departure1 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId1, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId2, channel = Api)

        running(app) {
          started(app).futureValue

          val respository   = app.injector.instanceOf[DepartureRepository]
          val allDepartures = Seq(departure1, departure2)
          val jsonArr       = allDepartures.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val result = respository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, None).futureValue

          result mustBe ResponseDepartures(Seq.empty, 0, 0, 0)
        }
      }

      "Must return max 2 departures when the API maxRowsReturned = 2" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value

        val now        = LocalDateTime.now(clock)

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(1))
        val departure2 = arbitrary[Departure].sample.value.copy(departureId = departureId2, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(2))
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(3))

        val departure2WithoutMessage = departureWithoutMessages(departure2)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[DepartureRepository]
          service.insert(departure1).futureValue
          service.insert(departure2).futureValue
          service.insert(departure3).futureValue

          val maxRows = appConfig.maxRowsReturned(Api)
          maxRows mustBe 2

          val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, updatedSince = None).futureValue

          departures.retrievedDepartures mustBe maxRows

          departures mustBe ResponseDepartures(Seq(departure3WithoutMessage, departure2WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 2, 3, 3)
        }
      }

      "Must return max 2 departures when the WEB maxRowsReturned = 1" in {
        database.flatMap(_.drop()).futureValue

        val app                = new GuiceApplicationBuilder().build()
        val eoriNumber: String = arbitrary[String].sample.value

        val now        = LocalDateTime.now(clock)
        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(1))
        val departure2 = arbitrary[Departure].sample.value.copy(departureId = departureId2, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(2))
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(3))

        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue
          val repository = app.injector.instanceOf[DepartureRepository]
          service.insert(departure1).futureValue
          service.insert(departure2).futureValue
          service.insert(departure3).futureValue

          val maxRows = appConfig.maxRowsReturned(Web)
          maxRows mustBe 1

          val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, updatedSince = None).futureValue

          departures.retrievedDepartures mustBe maxRows

          departures mustBe ResponseDepartures(Seq(departure3WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 1, 3, 3)
        }
      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {
        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value

        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(clock))
          .configure("metrics.jvm" -> false)
          .build()

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31))
        val departure2 = arbitrary[Departure].sample.value.copy(departureId = departureId2, eoriNumber = eoriNumber, channel = Api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32))
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Api, lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 21))
        val departure4 = arbitrary[Departure].sample.value.copy(departureId = departureId4, eoriNumber = eoriNumber, channel = Api, lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16))

        val departure2WithoutMessage = departureWithoutMessages(departure2)
        val departure4WithoutMessage = departureWithoutMessages(departure4)

        running(app) {
          started(app).futureValue

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val allMovements = Seq(departure1, departure2, departure3, departure4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val dateTime   = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
          val departures = service.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, Some(dateTime)).futureValue

          departures mustBe ResponseDepartures(Seq(departure4WithoutMessage, departure2WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 2, 4, 2)
        }
      }

      "must filter results by lrn when lrn search parameter provided matches" in {

        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(clock))
          .configure("metrics.jvm" -> false)
          .build()

        val departure1 = arbitrary[Departure].sample.value.copy(
          departureId = departureId1,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31),
          referenceNumber = lrn
        )
        val departure2 = arbitrary[Departure].sample.value.copy(
          departureId = departureId2,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 5, 30, 9, 35, 32),
          referenceNumber = lrn
        )
        val departure3 = arbitrary[Departure].sample.value.copy(
          departureId = departureId3,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 6, 30, 9, 30, 21),
          referenceNumber = lrn
        )
        val departure4 = arbitrary[Departure].sample.value.copy(
          departureId = departureId4,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 7, 30, 10, 15, 16),
          referenceNumber = lrn
        )

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure2WithoutMessage = departureWithoutMessages(departure2)
        val departure3WithoutMessage = departureWithoutMessages(departure3)
        val departure4WithoutMessage = departureWithoutMessages(departure4)

        val departuresWithoutMessages = Seq(departure4WithoutMessage, departure3WithoutMessage, departure2WithoutMessage, departure1WithoutMessage)

        running(app) {
          started(app).futureValue

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val allMovements = Seq(departure1, departure2, departure3, departure4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val departures = service.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn), Some(5)).futureValue

          departures mustBe ResponseDepartures(departuresWithoutMessages.map(ResponseDeparture.fromDepartureWithoutMessage), 4, 4, 4)
        }
      }

      "must filter results by lrn when substring of lrn search parameter provided matches" in {

        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(clock))
          .configure("metrics.jvm" -> false)
          .build()

        val departure1 = arbitrary[Departure].sample.value.copy(
          departureId = departureId1,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31),
          referenceNumber = lrn
        )
        val departure2 = arbitrary[Departure]
          .suchThat(_.referenceNumber != lrn)
          .sample
          .value
          .copy(
            departureId = departureId2,
            eoriNumber = eoriNumber,
            channel = Web,
            lastUpdated = LocalDateTime.of(2021, 5, 30, 9, 35, 32)
          )
        val departure3 = arbitrary[Departure].sample.value.copy(
          departureId = departureId3,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 6, 30, 9, 30, 21),
          referenceNumber = lrn
        )
        val departure4 = arbitrary[Departure]
          .suchThat(_.referenceNumber != lrn)
          .sample
          .value
          .copy(
            departureId = departureId4,
            eoriNumber = eoriNumber,
            channel = Web,
            lastUpdated = LocalDateTime.of(2021, 7, 30, 10, 15, 16)
          )

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val allMovements = Seq(departure1, departure2, departure3, departure4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val departures = service.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn.substring(2, 6)), Some(5)).futureValue

          departures mustBe ResponseDepartures(Seq(departure3WithoutMessage, departure1WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 2, 4, 2)
        }
      }

      "must filter results by lrn when substring of lrn search parameter is case insensitive provided matches" in {

        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(clock))
          .configure("metrics.jvm" -> false)
          .build()

        val departure1 = arbitrary[Departure].sample.value.copy(
          departureId = departureId1,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31),
          referenceNumber = lrn
        )
        val departure2 = arbitrary[Departure]
          .suchThat(_.referenceNumber != lrn)
          .sample
          .value
          .copy(
            departureId = departureId2,
            eoriNumber = eoriNumber,
            channel = Web,
            lastUpdated = LocalDateTime.of(2021, 5, 30, 9, 35, 32)
          )
        val departure3 = arbitrary[Departure].sample.value.copy(
          departureId = departureId3,
          eoriNumber = eoriNumber,
          channel = Web,
          lastUpdated = LocalDateTime.of(2021, 6, 30, 9, 30, 21),
          referenceNumber = lrn
        )
        val departure4 = arbitrary[Departure]
          .suchThat(_.referenceNumber != lrn)
          .sample
          .value
          .copy(
            departureId = departureId4,
            eoriNumber = eoriNumber,
            channel = Web,
            lastUpdated = LocalDateTime.of(2021, 7, 30, 10, 15, 16)
          )

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        running(app) {
          started(app).futureValue

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val allMovements = Seq(departure1, departure2, departure3, departure4)

          val jsonArr = allMovements.map(Json.toJsObject(_))

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val departures = service.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn.substring(2, 6).toLowerCase()), Some(5)).futureValue

          departures mustBe ResponseDepartures(Seq(departure3WithoutMessage, departure1WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 2, 4, 2)
        }
      }

      "must fetch all results based on pageSize 5 for page number 2" in {
        database.flatMap(_.drop()).futureValue

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

        lazy val allDepartures = nonEmptyListOfSize[Departure](20)((departure, id) => departure.copy(departureId = DepartureId(id)))
          .map(_.toList)
          .sample
          .value
          .map(
            _.copy(
              eoriNumber = eoriNumber,
              channel = Web,
              referenceNumber = lrn
            )
          )

        val pageSize = 5
        val page     = 2
        val app = new GuiceApplicationBuilder()
          .overrides(bind[Clock].toInstance(clock))
          .configure("metrics.jvm" -> false)
          .build()

        val departuresWithoutMessage = allDepartures.map(departureWithoutMessages)

        running(app) {
          started(app).futureValue

          val service: DepartureRepository = app.injector.instanceOf[DepartureRepository]

          val jsonArr = allDepartures.map(Json.toJsObject(_))

          val expectedAllDepartures = departuresWithoutMessage.map(ResponseDeparture.fromDepartureWithoutMessage).sortBy(_.updated)(_ compareTo _).reverse.slice(5, 10)

          database.flatMap {
            db =>
              db.collection[JSONCollection](DepartureRepository.collectionName).insert(false).many(jsonArr)
          }.futureValue

          val departures = service.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, None, Some(pageSize), Some(page)).futureValue

          departures mustBe ResponseDepartures(expectedAllDepartures, pageSize, allDepartures.size, allDepartures.size)
        }
      }
    }

    "getMessage" - {
      "must return Some(message) if departure and message exists" in {
        database.flatMap(_.drop()).futureValue

        val message   = arbitrary[models.MessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages  = new NonEmptyList(message, Nil)
        val departure = arbitrary[Departure].sample.value.copy(channel = Api, messages = messages)

        service.insert(departure).futureValue
        val result = service.getMessage(departure.departureId, departure.channel, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe true
            r.value mustEqual message
        }
      }

      "must return None if departure does not exist" in {
        database.flatMap(_.drop()).futureValue

        val result = service.getMessage(DepartureId(1), Api, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None if message does not exist" in {
        database.flatMap(_.drop()).futureValue

        val message   = arbitrary[models.MessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages  = new NonEmptyList(message, Nil)
        val departure = arbitrary[Departure].sample.value.copy(channel = Api, messages = messages)

        service.insert(departure).futureValue
        val result = service.getMessage(departure.departureId, departure.channel, MessageId(5))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }
  }
}
