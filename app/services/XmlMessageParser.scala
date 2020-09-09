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

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime

import cats.data.ReaderT
import cats.implicits._
import models.MessageType
import models.MovementReferenceNumber
import models.ParseError
import models.ParseError._
import utils.Format

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml.NodeSeq

object XmlMessageParser {

  type ParseHandler[A] = Either[ParseError, A]

  def correctRootNodeR(messageType: MessageType): ReaderT[ParseHandler, NodeSeq, Unit] =
    ReaderT[ParseHandler, NodeSeq, Unit] {
      nodeSeq =>
        nodeSeq.head.label match {
          case messageType.rootNode => Right(nodeSeq)
          case _                    => Left(InvalidRootNode(s"Node ${nodeSeq.head.label} didn't match ${messageType.rootNode}"))
        }
    }

  val dateOfPrepR: ReaderT[ParseHandler, NodeSeq, LocalDate] =
    ReaderT[ParseHandler, NodeSeq, LocalDate](xml => {

      val dateOfPrepString = (xml \ "DatOfPreMES9").text

      Try(LocalDate.parse(dateOfPrepString, Format.dateFormatter)) match {
        case Success(value) => Right(value)
        case Failure(e)     => Left(LocalDateParseFailure(s"Failed to parse DatOfPreMES9 to LocalDate with error: ${e.getMessage}"))
      }
    })

  val timeOfPrepR: ReaderT[ParseHandler, NodeSeq, LocalTime] =
    ReaderT[ParseHandler, NodeSeq, LocalTime](xml => {

      val timeOfPrepString = (xml \ "TimOfPreMES10").text

      Try(LocalTime.parse(timeOfPrepString, Format.timeFormatter)) match {
        case Success(value) => Right(value)
        case Failure(e)     => Left(LocalTimeParseFailure(s"Failed to parse TimOfPreMES10 to LocalTime with error: ${e.getMessage}"))
      }
    })

  val dateTimeOfPrepR: ReaderT[ParseHandler, NodeSeq, LocalDateTime] =
    for {
      date <- dateOfPrepR
      time <- timeOfPrepR
    } yield LocalDateTime.of(date, time)

  val referenceR: ReaderT[ParseHandler, NodeSeq, String] =
    ReaderT[ParseHandler, NodeSeq, String](xml =>
      (xml \ "HEAHEA" \ "RefNumHEA4").text match {
        case refString if !refString.isEmpty => Right(refString)
        case _                               => Left(EmptyLocalReferenceNumber("RefNumHEA4 was empty"))
    })

  val mrnR: ReaderT[ParseHandler, NodeSeq, MovementReferenceNumber] =
    ReaderT[ParseHandler, NodeSeq, MovementReferenceNumber](xml =>
      (xml \ "HEAHEA" \ "DocNumHEA5").text match {
        case mrnString if !mrnString.isEmpty => Right(MovementReferenceNumber(mrnString))
        case _                               => Left(EmptyMovementReferenceNumber("DocNumHEA5 was empty"))
    })
}
