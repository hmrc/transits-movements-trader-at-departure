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

package services

import cats.data._
import cats.implicits._
import com.google.inject.Inject
import models.DepartureStatus.Initialized
import models.MessageStatus.SubmissionPending
import models._
import models.ParseError.EmptyNodeSeq
import repositories.DepartureIdRepository
import utils.XMLTransformer

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class DepartureService @Inject()(departureIdRepository: DepartureIdRepository)(implicit ec: ExecutionContext) {
  import XMLTransformer._
  import XmlMessageParser._

  def makeMessageWithStatus(departureId: DepartureId, messageCorrelationId: Int, messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- updateMesSenMES3(departureId, messageCorrelationId)
    } yield MessageWithStatus(dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)

  def createDeparture(eori: String, nodeSeq: NodeSeq, channelType: ChannelType): Future[ParseHandler[Departure]] =
    departureIdRepository
      .nextId()
      .map {
        departureId =>
          (for {
            _         <- correctRootNodeR(MessageType.DepartureDeclaration)
            dateTime  <- dateTimeOfPrepR
            reference <- referenceR
            message   <- makeMessageWithStatus(departureId, 1, MessageType.DepartureDeclaration)
          } yield {
            Departure(
              departureId,
              channelType,
              eori,
              None,
              reference,
              Initialized,
              dateTime,
              dateTime,
              2,
              NonEmptyList.one(message)
            )
          }).apply(nodeSeq)
      }

  def makeMessage(messageCorrelationId: Int, messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MessageWithoutStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[ParseHandler, NodeSeq, NodeSeq](nodeSeqToEither)
    } yield MessageWithoutStatus(dateTime, messageType, xmlMessage, messageCorrelationId)

  private[this] def nodeSeqToEither(xml: NodeSeq): ParseHandler[NodeSeq] =
    if (xml != null) {
      Right(xml)
    } else {
      Left(EmptyNodeSeq("Request body is empty"))
    }
}
