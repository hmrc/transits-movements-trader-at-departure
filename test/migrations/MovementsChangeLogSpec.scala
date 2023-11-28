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

package migrations

import java.time._

class MovementsChangeLogSpec {
  /*  extends SpecBase
    with IntegrationPatience
    with BeforeAndAfterAll
    with GuiceOneAppPerSuite
    with DefaultPlayMongoRepositorySupport[Departure] {

  val departureIds             = (1 to 20).map(DepartureId.apply)
  val eori                     = "123456789000"
  private val config           = app.injector.instanceOf[Configuration]
  private val appConfig        = app.injector.instanceOf[AppConfig]
  val localDate                = LocalDate.now()
  val localTime                = LocalTime.of(1, 1)
  val localDateTime            = LocalDateTime.of(localDate, localTime)
  implicit val clock           = Clock.fixed(localDateTime.toInstant(ZoneOffset.UTC), ZoneOffset.UTC)
  override lazy val repository = new DepartureRepositoryImpl(mongoComponent, appConfig, config, new TestMetrics())

  override def beforeAll(): Unit = {

    val messageWithStatus = MessageWithStatus(
      MessageId(0),
      LocalDateTime.of(2021, 7, 15, 12, 12),
      Some(LocalDateTime.of(2021, 7, 15, 12, 12)),
      MessageType.DepartureDeclaration,
      NodeSeq.fromSeq(Seq(<CC015B></CC015B>)),
      MessageStatus.SubmissionSucceeded,
      1
    )
    val messageWithoutStatus1 = MessageWithoutStatus(
      MessageId(-1),
      LocalDateTime.of(2021, 7, 15, 12, 12),
      Some(LocalDateTime.of(2021, 7, 15, 12, 12)),
      MessageType.PositiveAcknowledgement,
      NodeSeq.fromSeq(Seq(<CC928A></CC928A>)),
      1
    )
    val messageWithoutStatus2 = MessageWithoutStatus(
      MessageId(-1),
      LocalDateTime.of(2021, 7, 15, 12, 12),
      Some(LocalDateTime.of(2021, 7, 15, 12, 12)),
      MessageType.MrnAllocated,
      NodeSeq.fromSeq(Seq(<CC028A></CC028A>)),
      1
    )

    val departure = Departure(
      DepartureId(1),
      ChannelType.Web,
      eori,
      None,
      Random.alphanumeric.take(20).mkString,
      LocalDateTime.of(2021, 7, 15, 12, 12),
      LocalDateTime.of(2021, 7, 15, 12, 13),
      2,
      NonEmptyList(messageWithStatus, List(messageWithoutStatus1, messageWithoutStatus2)),
      None
    )
    repository.insert(departure).futureValue

    val departure1 = repository.get(DepartureId(1)).futureValue
    println("++++++++QQQ" + departure1)
  }

  "MovementsChangeLog" - {
    "addMessageIdToMessages" - {
      "should add message ID to messages in the departures collection" in {
        // val repo   = app.injector.instanceOf[DepartureRepository]
        val runner = app.injector.instanceOf[MigrationRunnerImpl]

        runner.runMigrations().futureValue

        val departure = repository.get(DepartureId(1)).futureValue
        println("++++++++" + departure)

      }
    }
  }*/
}
