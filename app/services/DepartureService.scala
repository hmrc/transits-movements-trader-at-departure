/*
 * Copyright 2020 HM Revenue & Customs
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

package services

import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.DepartureStatus.Initialized
import models.MessageStatus.SubmissionPending
import models.{Departure, MessageType, MessageWithStatus}
import repositories.DepartureIdRepository

import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

class DepartureService @Inject()(departureIdRepository: DepartureIdRepository)(implicit ec: ExecutionContext) {
  import XmlMessageParser._

  def makeMessageWithStatus(messageCorrelationId: Int, messageType: MessageType): ReaderT[Option, NodeSeq, MessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[Option, NodeSeq, NodeSeq](Option.apply)
    } yield MessageWithStatus(dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)

  def createDeparture(eori: String): ReaderT[Option, NodeSeq, Future[Departure]] =
    for {
      _        <- correctRootNodeR(MessageType.DepartureDeclaration)
      dateTime <- dateTimeOfPrepR
      reference <- referenceR
      message  <- makeMessageWithStatus(1, MessageType.DepartureDeclaration)
    } yield {
      departureIdRepository
        .nextId()
        .map(
          Departure(
            _,
            eori,
            None,
            reference,
            Initialized,
            dateTime,
            dateTime,
            2,
            NonEmptyList.one(message)
          ))
    }
}
