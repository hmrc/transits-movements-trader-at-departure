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
import models.ParseError.EmptyNodeSeq
import models._
import repositories.DepartureIdRepository
import utils.XMLTransformer
import utils.XmlToJsonConverter
import java.time.Clock
import java.time.LocalDateTime

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class DepartureService @Inject()(departureIdRepository: DepartureIdRepository, xmlToJsonConverter: XmlToJsonConverter)(implicit clock: Clock,
                                                                                                                       ec: ExecutionContext) {
  import XMLTransformer._
  import XmlMessageParser._

  def makeMessageWithStatus(departureId: DepartureId,
                            messageId: MessageId,
                            messageCorrelationId: Int,
                            messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MessageWithStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- updateMesSenMES3(departureId, messageCorrelationId)
    } yield MessageWithStatus(messageId, dateTime, messageType, xmlMessage, SubmissionPending, messageCorrelationId)(xmlToJsonConverter)

  def createDeparture(enrolmentId: Ior[TURN, EORINumber], nodeSeq: NodeSeq, channelType: ChannelType, boxOpt: Option[Box]): Future[ParseHandler[Departure]] =
    departureIdRepository
      .nextId()
      .map {
        departureId =>
          (for {
            _         <- correctRootNodeR(MessageType.DepartureDeclaration)
            dateTime  <- dateTimeOfPrepR
            reference <- referenceR
            message   <- makeMessageWithStatus(departureId, MessageId(1), 1, MessageType.DepartureDeclaration)
          } yield
            Departure(
              departureId,
              channelType,
              // Prefer to use EORI number
              enrolmentId.fold(
                turn => turn.value,
                eoriNumber => eoriNumber.value,
                (_, eoriNumber) => eoriNumber.value
              ),
              None,
              reference,
              Initialized,
              dateTime,
              LocalDateTime.now(clock),
              2,
              NonEmptyList.one(message),
              boxOpt
            )).apply(nodeSeq)
      }

  def makeMessage(messageId: MessageId, messageCorrelationId: Int, messageType: MessageType): ReaderT[ParseHandler, NodeSeq, MessageWithoutStatus] =
    for {
      _          <- correctRootNodeR(messageType)
      dateTime   <- dateTimeOfPrepR
      xmlMessage <- ReaderT[ParseHandler, NodeSeq, NodeSeq](nodeSeqToEither)
    } yield MessageWithoutStatus(messageId, dateTime, messageType, xmlMessage, messageCorrelationId)(xmlToJsonConverter)

  private[this] def nodeSeqToEither(xml: NodeSeq): ParseHandler[NodeSeq] =
    if (xml != null) {
      Right(xml)
    } else {
      Left(EmptyNodeSeq("Request body is empty"))
    }
}
