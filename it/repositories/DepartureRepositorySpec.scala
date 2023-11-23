/*
 * Copyright 2023 HM Revenue & Customs
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
import com.mongodb.client.model.Filters
import config.AppConfig
import generators.ModelGenerators
import models.ChannelType.Api
import models.ChannelType.Web
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models._
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest._
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.Configuration
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport
import utils.Format
import utils.JsonHelper
import utils.TestMetrics

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
    with GuiceOneAppPerSuite
    with MongoDateTimeFormats
    with JsonHelper
    with MockitoSugar
    with DefaultPlayMongoRepositorySupport[Departure] {

  implicit override lazy val app: Application = GuiceApplicationBuilder()
    .configure("feature-flags.testOnly.enabled" -> true)
    .build()

  private val config            = app.injector.instanceOf[Configuration]
  private val appConfig         = app.injector.instanceOf[AppConfig]
  val localDate                 = LocalDate.now()
  val localTime                 = LocalTime.of(1, 1)
  val localDateTime             = LocalDateTime.of(localDate, localTime)
  implicit val clock            = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
  override lazy val repository  = new DepartureRepositoryImpl(mongoComponent, appConfig, config, new TestMetrics())
  override def afterAll(): Unit = dropDatabase()

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
    Gen
      .listOfN(size, arbitrary[T])
      // Don't generate duplicate IDs
      .map(_.foldLeft((Chain.empty[T], 1)) {
        case ((ts, id), t) =>
          (ts :+ f(t, id), id + 1)
      })
      .map {
        case (ts, _) =>
          NonEmptyList.fromListUnsafe(ts.toList)
      }

  val departureWithOneMessage: Gen[Departure] = for {
    departure <- arbitrary[Departure]
    message   <- arbitrary[MessageWithStatus]
  } yield departure.copy(messages = NonEmptyList.one(message.copy(status = SubmissionPending)))

  private def departureWithoutMessages(departure: Departure): DepartureWithoutMessages =
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
      departure.messages
        .map(
          x => MessageMetaData(x.messageType, x.dateTime)
        )
        .toList
    )

  private val departureId1 = arbitrary[DepartureId].sample.value
  private val departureId2 = departureId1.copy(index = departureId1.index + 1)
  private val departureId3 = departureId2.copy(index = departureId2.index + 1)
  private val departureId4 = departureId3.copy(index = departureId3.index + 1)

  "DepartureRepository" - {
    "insert" - {
      "must persist Departure within mongoDB" in {

        val departure = arbitrary[Departure].sample.value

        repository.insert(departure).futureValue

        val selector = Filters.eq("_id", departure.departureId)

        val result = repository.collection.find(selector).head()

        whenReady(result) {
          r =>
            r mustBe departure
        }
      }
    }

    "getMaxDepartureId" - {
      "must return the highest departure id in the database" in {

        val departures = List.tabulate(5)(
          index => arbitrary[Departure].sample.value.copy(departureId = DepartureId(index + 1))
        )

        repository.bulkInsert(departures).futureValue

        repository.getMaxDepartureId.futureValue.value mustBe DepartureId(5)
      }
    }

    "get(departureId: DepartureId)" - {
      "must get an departure when it exists and has the right channel type" in {

        val departure = arbitrary[Departure].sample.value

        repository.insert(departure).futureValue
        val result = repository.get(departure.departureId)

        whenReady(result) {
          r =>
            r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.get(DepartureId(2))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "getMessagesOfType(arrivalId: ArrivalId, channelFilter: ChannelType, messageTypes: List[MessageType])" - {

      val mType = MessageType.ReleaseForTransit

      val node = NodeSeq.fromSeq(Seq(<CC029B>m</CC029B>))

      val dateOfPrep = LocalDate.now(clock)
      val timeOfPrep = LocalTime.of(1, 1, 1)
      val dateTime   = LocalDateTime.of(dateOfPrep, timeOfPrep)

      val message = MessageWithStatus(
        MessageId(1),
        dateTime,
        Some(dateTime),
        MessageType.ReleaseForTransit,
        node,
        MessageStatus.SubmissionSucceeded,
        1
      )

      val otherMessage = MessageWithStatus(
        MessageId(2),
        dateTime,
        Some(dateTime),
        MessageType.NoReleaseForTransit,
        node,
        MessageStatus.SubmissionSucceeded,
        2
      )

      "must get the appropriate messages when they exist and has the right channel type" in {

        val departure = arbitrary[Departure].sample.value copy (messages = NonEmptyList[Message](message, List.empty))

        repository.insert(departure).futureValue

        // We copy the message node because the returned node isn't equal, even though it's
        // identical for our purposes. As it's not what we are really testing, we just copy the
        // original message across so it doesn't fail the equality check
        val result = repository
          .getMessagesOfType(departure.departureId, departure.channel, List(mType))
          .map(
            opt =>
              opt.map(
                ar =>
                  ar.messages
                    .asInstanceOf[List[MessageWithStatus]]
                    .map(
                      x => x copy (message = node)
                    )
              )
          )

        whenReady(result) {
          r =>
            r mustBe defined
            r.get must contain theSameElementsAs List(message)
        }
      }

      "must only return the appropriate messages when an arrival is matched" in {

        val departure = arbitrary[Departure].sample.value copy (messages = NonEmptyList[Message](message, List(otherMessage)))

        repository.insert(departure).futureValue

        // As in the previous test.
        val result = repository
          .getMessagesOfType(departure.departureId, departure.channel, List(mType))
          .map(
            opt =>
              opt.map(
                ar =>
                  ar.messages
                    .asInstanceOf[List[MessageWithStatus]]
                    .map(
                      x => x copy (message = node)
                    )
              )
          )

        whenReady(result) {
          r =>
            r mustBe defined
            r.get must contain theSameElementsAs List(message)
        }
      }

      "must return an empty list when an arrival exists but no messages match" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), messages = NonEmptyList[Message](otherMessage, List.empty))

        repository.insert(departure).futureValue
        val result = repository.getMessagesOfType(DepartureId(1), departure.channel, List(mType))

        whenReady(result) {
          r =>
            r mustBe defined
            r.get.messages mustEqual List()
        }
      }

      "must return None when an arrival does not exist" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.getMessagesOfType(DepartureId(2), departure.channel, List(mType))

        whenReady(result) {
          r =>
            r must not be defined
        }
      }

      "must return an empty list when an arrival exists but without any of the message type" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.getMessagesOfType(DepartureId(2), departure.channel, List(mType))

        whenReady(result) {
          r =>
            r mustBe empty
        }
      }

      "must return None when an arrival does exist but with the wrong channel type" in {

        val departure = arbitrary[Departure].sample.value copy (channel = ChannelType.Api)

        repository.insert(departure).futureValue
        val result = repository.getMessagesOfType(departure.departureId, ChannelType.Web, List(mType))

        whenReady(result) {
          r =>
            r must not be defined
        }
      }

    }

    "get(departureId: DepartureId, channelFilter: ChannelType)" - {
      "must get an departure when it exists and has the right channel type" in {

        val departure = arbitrary[Departure].sample.value.copy(channel = Api)

        repository.insert(departure).futureValue
        val result = repository.get(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value mustEqual departure
        }
      }

      "must return None when an departure does not exist" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), channel = Api)

        repository.insert(departure).futureValue
        val result = repository.get(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        repository.insert(departure).futureValue
        val result = repository.get(DepartureId(1), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "getWithoutMessages(departureId: DepartureId)" - {
      "must get an departure when it exists" in {
        val departure = arbitrary[Departure].sample.value

        repository.insert(departure).futureValue
        val result = repository.getWithoutMessages(departure.departureId)

        whenReady(result) {
          r =>
            r.value mustEqual departureWithoutMessages(departure)
        }
      }

      "must return None when an departure does not exist" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.getWithoutMessages(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        repository.insert(departure).futureValue
        val result = repository.get(DepartureId(1), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "getWithoutMessages(departureId: DepartureId, channelFilter: ChannelType)" - {
      "must get an departure when it exists and has the right channel type" in {

        val departure = arbitrary[Departure].sample.value.copy(channel = Api)

        repository.insert(departure).futureValue
        val result = repository.getWithoutMessages(departure.departureId, departure.channel)

        whenReady(result) {
          r =>
            r.value mustEqual departureWithoutMessages(departure)
        }
      }

      "must return None when an departure does not exist" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), channel = Api)

        repository.insert(departure).futureValue
        val result = repository.getWithoutMessages(DepartureId(2), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None when a departure exists, but with a different channel type" in {

        val departure = arbitrary[Departure].sample.value copy (departureId = DepartureId(1), Api)

        repository.insert(departure).futureValue
        val result = repository.get(DepartureId(1), Web)

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }

    "setMessageState" - {
      "must update the status of a specific message in an existing departure" in {

        val departure = departureWithOneMessage.sample.value

        repository.insert(departure).futureValue

        repository.setMessageState(departure.departureId, 0, SubmissionSucceeded).futureValue

        val updatedDeparture = repository.get(departure.departureId, departure.channel)

        whenReady(updatedDeparture) {
          r =>
            typeMatchOnTestValue(r.value.messages.head) {
              result: MessageWithStatus =>
                result.status mustEqual SubmissionSucceeded
            }
        }
      }

      "must fail if the departure cannot be found" in {

        val departure = departureWithOneMessage.sample.value.copy(departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.setMessageState(DepartureId(2), 0, SubmissionSucceeded)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }

      "must fail if the message doesn't exist" in {

        val departure = departureWithOneMessage.sample.value copy (departureId = DepartureId(1))

        repository.insert(departure).futureValue
        val result = repository.setMessageState(DepartureId(1), 5, SubmissionSucceeded)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }

      "must fail if the message does not have a status" in {

        val preGenDeparture = departureWithOneMessage.sample.value
        val departure       = preGenDeparture.copy(departureId = DepartureId(1), messages = NonEmptyList.one(arbitrary[MessageWithoutStatus].sample.value))

        repository.insert(departure).futureValue
        val result = repository.setMessageState(DepartureId(1), 0, SubmissionSucceeded)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }
    }

    "addNewMessage" - {
      "must add a message, update the timestamp and increment nextCorrelationId" in {

        val declarationMessages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2, 2),
            Some(LocalDateTime.of(2021, 2, 2, 2, 2, 2)),
            MessageType.DepartureDeclaration,
            <CC015></CC015>,
            MessageStatus.SubmissionPending,
            1
          )
        )
        val departure = arbitrary[Departure].sample.value.copy(messages = declarationMessages, lastUpdated = LocalDateTime.of(2021, 2, 2, 2, 2, 2))

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
            Some(dateTime),
            MessageType.DepartureDeclaration,
            messageBody,
            departure.nextMessageCorrelationId
          )

        repository.insert(departure).futureValue
        repository.addNewMessage(departure.departureId, departureDeclarationMessage).futureValue.success

        val selector = Filters.eq("_id" -> departure.departureId)

        val result = repository.collection.find(selector).head()

        whenReady(result) {
          updatedDeparture =>
            updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 1
            updatedDeparture.status mustEqual departure.status
            updatedDeparture.messages.size - departure.messages.size mustEqual 1
            updatedDeparture.messages.last mustEqual departureDeclarationMessage
        }
      }

      "must fail if the departure cannot be found" in {

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            Some(LocalDateTime.of(2021, 2, 2, 2, 2)),
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
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.DepartureDeclaration,
            messageBody,
            messageCorrelationId = 1
          )

        repository.insert(departure).futureValue
        val result = repository.addNewMessage(DepartureId(2), departureDeclaration)

        whenReady(result) {
          r =>
            r mustBe a[Failure[_]]
        }
      }
    }

    "addResponseMessage" - {
      "must add a message, update the status of a document and update the timestamp" in {

        val departure = arbitrary[Departure].sample.value

        val dateOfPrep = LocalDate.now(clock)
        val timeOfPrep = LocalTime.of(1, 1, 1)
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
            Some(dateTime),
            MessageType.DeclarationRejected,
            messageBody,
            departure.nextMessageCorrelationId
          )

        repository.insert(departure).futureValue
        val addMessageResult = repository.addResponseMessage(departure.departureId, declarationRejectedMessage, dateTime).futureValue

        val selector = Filters.eq("_id", departure.departureId)

        val updatedDeparture = repository.collection.find(selector).head().futureValue
        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.status mustEqual DepartureStatus.DepartureRejected
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual declarationRejectedMessage
      }
      "must fail if the departure cannot be found" in {

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            Some(LocalDateTime.of(2021, 2, 2, 2, 2)),
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
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.DeclarationRejected,
            messageBody,
            messageCorrelationId = 1
          )

        repository.insert(departure).futureValue
        val result = repository.addResponseMessage(DepartureId(2), declarationRejected, LocalDateTime.of(dateOfPrep, timeOfPrep)).futureValue

        result mustBe a[Failure[_]]
      }
    }

    "setMrnAndAddResponseMessage" - {
      "must add a message, update the status of a document, update the timestamp, and update the MRN" in {

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
            Some(dateTime),
            MessageType.MrnAllocated,
            messageBody,
            departure.nextMessageCorrelationId
          )

        repository.insert(departure).futureValue
        val addMessageResult =
          repository.setMrnAndAddResponseMessage(departure.departureId, mrnAllocatedMessage, MovementReferenceNumber(mrn), dateTime).futureValue

        val selector = Filters.eq("_id", departure.departureId)

        val updatedDeparture = repository.collection.find(selector).head().futureValue

        addMessageResult mustBe a[Success[_]]
        updatedDeparture.nextMessageCorrelationId - departure.nextMessageCorrelationId mustBe 0
        updatedDeparture.status mustEqual DepartureStatus.MrnAllocated
        updatedDeparture.movementReferenceNumber.get mustEqual MovementReferenceNumber(mrn)
        updatedDeparture.messages.size - departure.messages.size mustEqual 1
        updatedDeparture.messages.last mustEqual mrnAllocatedMessage
      }

      "must fail if the departure cannot be found" in {

        val messages = NonEmptyList.one(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.of(2021, 2, 2, 2, 2),
            Some(LocalDateTime.of(2021, 2, 2, 2, 2)),
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
            Some(LocalDateTime.of(dateOfPrep, timeOfPrep)),
            MessageType.MrnAllocated,
            messageBody,
            departure.nextMessageCorrelationId
          )

        repository.insert(departure).futureValue
        val addMessageResult = repository
          .setMrnAndAddResponseMessage(DepartureId(2), mrnAllocatedMessage, MovementReferenceNumber(mrn), LocalDateTime.of(dateOfPrep, timeOfPrep))
          .futureValue

        addMessageResult mustBe a[Failure[_]]
      }
    }

    "fetchAllDepartures" - {

      "return DeparturesWithoutMessages that match an eoriNumber and channel type" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId2, channel = Api)
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Web)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        val departures = Seq(departure1, departure2, departure3)

        repository.collection.insertMany(departures).toFuture().futureValue

        repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)),
          1,
          1,
          1
        )
        repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)),
          1,
          1,
          1
        )
      }

      "return DeparturesWithoutMessages with eoriNumber that match legacy TURN and channel type" in {

        val turn: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = turn, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != turn).sample.value.copy(channel = Api, departureId = departureId2)
        val departure3 = arbitrary[Departure].sample.value.copy(eoriNumber = turn, channel = Web, departureId = departureId3)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        val departures = Seq(departure1, departure2, departure3)

        repository.collection.insertMany(departures).toFuture().futureValue

        repository.fetchAllDepartures(Ior.left(TURN(turn)), Api, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)),
          1,
          1,
          1
        )
        repository.fetchAllDepartures(Ior.left(TURN(turn)), Web, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)),
          1,
          1,
          1
        )
      }

      "return DeparturesWithoutMessages with eoriNumber that match either eoriNumber or legacy TURN and channel type" in {
        val eori: String     = arbitrary[String].sample.value
        val turn: String     = arbitrary[String].sample.value
        val ids: Set[String] = Set(eori, turn)

        val departure1 = arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eori, channel = Api)
        val departure2 = arbitrary[Departure]
          .suchThat(
            departure => !ids.contains(departure.eoriNumber)
          )
          .sample
          .value
          .copy(departureId = departureId2, channel = Api)
        val departure3 = arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = turn, channel = Web)

        val departure1WithoutMessage = departureWithoutMessages(departure1)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        val departures = Seq(departure1, departure2, departure3)
        repository.collection.insertMany(departures).toFuture().futureValue

        repository.fetchAllDepartures(Ior.both(TURN(turn), EORINumber(eori)), Api, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure1WithoutMessage)),
          1,
          1,
          1
        )
        repository.fetchAllDepartures(Ior.both(TURN(turn), EORINumber(eori)), Web, None).futureValue mustBe ResponseDepartures(
          Seq(ResponseDeparture.fromDepartureWithoutMessage(departure3WithoutMessage)),
          1,
          1,
          1
        )
      }

      "must return an empty sequence when there are no movements with the same eori" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val app        = new GuiceApplicationBuilder().configure("metrics.jvm" -> false).build()
        val departure1 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId1, channel = Api)
        val departure2 = arbitrary[Departure].suchThat(_.eoriNumber != eoriNumber).sample.value.copy(departureId = departureId2, channel = Api)

        val allDepartures = Seq(departure1, departure2)
        repository.collection.insertMany(allDepartures).toFuture().futureValue
        val result = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, None).futureValue

        result mustBe ResponseDepartures(Seq.empty, 0, 0, 0)
      }

      "Must return max 2 departures when the API maxRowsReturned = 2" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val now = LocalDateTime.now(clock)

        val departure1 =
          arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(1))
        val departure2 =
          arbitrary[Departure].sample.value.copy(departureId = departureId2, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(2))
        val departure3 =
          arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Api, lastUpdated = now.withSecond(3))

        val departure2WithoutMessage = departureWithoutMessages(departure2)
        val departure3WithoutMessage = departureWithoutMessages(departure3)

        repository.collection.insertOne(departure1).head().futureValue
        repository.collection.insertOne(departure2).head().futureValue
        repository.collection.insertOne(departure3).head().futureValue

        val maxRows = appConfig.maxRowsReturned(Api)
        maxRows mustBe 2

        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, updatedSince = None).futureValue

        departures.retrievedDepartures mustBe maxRows

        departures mustBe ResponseDepartures(
          Seq(departure3WithoutMessage, departure2WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage),
          2,
          3,
          3
        )
      }

      "Must return max 2 departures when the WEB maxRowsReturned = 1" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val now = LocalDateTime.now(clock)
        val departure1 =
          arbitrary[Departure].sample.value.copy(departureId = departureId1, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(1))
        val departure2 =
          arbitrary[Departure].sample.value.copy(departureId = departureId2, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(2))
        val departure3 =
          arbitrary[Departure].sample.value.copy(departureId = departureId3, eoriNumber = eoriNumber, channel = Web, lastUpdated = now.withSecond(3))

        val departure3WithoutMessage = departureWithoutMessages(departure3)

        repository.collection.insertOne(departure1).head().futureValue
        repository.collection.insertOne(departure2).head().futureValue
        repository.collection.insertOne(departure3).head().futureValue

        val maxRows = appConfig.maxRowsReturned(Web)
        maxRows mustBe 1

        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, updatedSince = None).futureValue

        departures.retrievedDepartures mustBe maxRows

        departures mustBe ResponseDepartures(Seq(departure3WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage), 1, 3, 3)
      }

      "must filter results by lastUpdated when updatedSince parameter is provided" in {

        val eoriNumber: String = arbitrary[String].sample.value

        val departure1 = arbitrary[Departure].sample.value.copy(
          departureId = departureId1,
          eoriNumber = eoriNumber,
          channel = Api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 31)
        )
        val departure2 = arbitrary[Departure].sample.value.copy(
          departureId = departureId2,
          eoriNumber = eoriNumber,
          channel = Api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 35, 32)
        )
        val departure3 = arbitrary[Departure].sample.value.copy(
          departureId = departureId3,
          eoriNumber = eoriNumber,
          channel = Api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 9, 30, 21)
        )
        val departure4 = arbitrary[Departure].sample.value.copy(
          departureId = departureId4,
          eoriNumber = eoriNumber,
          channel = Api,
          lastUpdated = LocalDateTime.of(2021, 4, 30, 10, 15, 16)
        )

        val departure2WithoutMessage = departureWithoutMessages(departure2)
        val departure4WithoutMessage = departureWithoutMessages(departure4)

        val allMovements = Seq(departure1, departure2, departure3, departure4)

        repository.collection.insertMany(allMovements).toFuture().futureValue

        val dateTime   = OffsetDateTime.of(LocalDateTime.of(2021, 4, 30, 10, 30, 32), ZoneOffset.ofHours(1))
        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Api, Some(dateTime)).futureValue

        departures mustBe ResponseDepartures(
          Seq(departure4WithoutMessage, departure2WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage),
          2,
          4,
          2
        )
      }

      "must filter results by lrn when lrn search parameter provided matches" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

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

        val allMovements = Seq(departure1, departure2, departure3, departure4)
        repository.collection.insertMany(allMovements).toFuture().futureValue

        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn), Some(5)).futureValue

        departures mustBe ResponseDepartures(departuresWithoutMessages.map(ResponseDeparture.fromDepartureWithoutMessage), 4, 4, 4)
      }

      "must filter results by lrn when substring of lrn search parameter provided matches" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

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

        val allMovements = Seq(departure1, departure2, departure3, departure4)

        repository.collection.insertMany(allMovements).toFuture().futureValue

        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn.substring(2, 6)), Some(5)).futureValue

        departures mustBe ResponseDepartures(
          Seq(departure3WithoutMessage, departure1WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage),
          2,
          4,
          2
        )
      }

      "must filter results by lrn when substring of lrn search parameter is case insensitive provided matches" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

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

        val allMovements = Seq(departure1, departure2, departure3, departure4)

        repository.collection.insertMany(allMovements).toFuture().futureValue

        val departures =
          repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(lrn.substring(2, 6).toLowerCase()), Some(5)).futureValue

        departures mustBe ResponseDepartures(
          Seq(departure3WithoutMessage, departure1WithoutMessage).map(ResponseDeparture.fromDepartureWithoutMessage),
          2,
          4,
          2
        )
      }

      "must return no results when an attempt at a regex is provided to the lrn search parameter" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

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

        val allMovements = Seq(departure1, departure2, departure3, departure4)
        repository.collection.insertMany(allMovements).toFuture().futureValue

        val departures =
          repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some("a.+"), Some(5)).futureValue

        departures mustBe ResponseDepartures(
          Seq(),
          0,
          4,
          0
        )
      }

      "must return no results when an invalid regex is provided" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

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

        val allMovements = Seq(departure1, departure2, departure3, departure4)
        repository.collection.insertMany(allMovements).toFuture().futureValue

        val departures =
          repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, Some(s"+$lrn"), Some(5)).futureValue

        departures mustBe ResponseDepartures(
          Seq(),
          0,
          4,
          0
        )
      }

      "must fetch all results based on pageSize 5 for page number 2" in {

        val eoriNumber: String = arbitrary[String].sample.value
        val lrn: String        = Gen.listOfN(10, Gen.alphaChar).map(_.mkString).sample.value

        lazy val allDepartures = nonEmptyListOfSize[Departure](20)(
          (departure, id) => departure.copy(departureId = DepartureId(id))
        )
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

        val departuresWithoutMessage = allDepartures.map(departureWithoutMessages)

        val expectedAllDepartures =
          departuresWithoutMessage.map(ResponseDeparture.fromDepartureWithoutMessage).sortBy(_.updated)(_ compareTo _).reverse.slice(5, 10)

        repository.collection.insertMany(allDepartures).toFuture().futureValue

        val departures = repository.fetchAllDepartures(Ior.right(EORINumber(eoriNumber)), Web, None, None, Some(pageSize), Some(page)).futureValue

        departures mustBe ResponseDepartures(expectedAllDepartures, pageSize, allDepartures.size, allDepartures.size)
      }

    }

    "getMessage" - {
      "must return Some(message) if departure and message exists" in {

        val message   = arbitrary[models.MessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages  = new NonEmptyList(message, Nil)
        val departure = arbitrary[Departure].sample.value.copy(channel = Api, messages = messages)

        repository.insert(departure).futureValue
        val result = repository.getMessage(departure.departureId, departure.channel, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe true
            r.value mustEqual message
        }
      }

      "must return None if departure does not exist" in {

        val result = repository.getMessage(DepartureId(1), Api, MessageId(1))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }

      "must return None if message does not exist" in {

        val message   = arbitrary[models.MessageWithStatus].sample.value.copy(messageId = MessageId(1))
        val messages  = new NonEmptyList(message, Nil)
        val departure = arbitrary[Departure].sample.value.copy(channel = Api, messages = messages)

        repository.insert(departure).futureValue
        val result = repository.getMessage(departure.departureId, departure.channel, MessageId(5))

        whenReady(result) {
          r =>
            r.isDefined mustBe false
        }
      }
    }
  }
}
