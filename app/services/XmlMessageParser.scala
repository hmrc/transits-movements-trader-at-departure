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

import cats.data.ReaderT
import cats.implicits._
import models.MessageType
import models.MovementReferenceNumber
import models.ParseError
import models.ParseError._
import utils.Format

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
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
          case messageType.rootNode => Right(())
          case _                    => Left(InvalidRootNode("The root element name does not match 'CC015B'"))
        }
    }

  val dateOfPrepR: ReaderT[ParseHandler, NodeSeq, LocalDate] =
    ReaderT[ParseHandler, NodeSeq, LocalDate](xml => {

      val dateOfPrepString = (xml \ "DatOfPreMES9").text

      val dateFormatterEither = dateOfPrepString.length match {
        case 6 => Right(Format.dateFormatter6)
        case 8 => Right(Format.dateFormatter8)
        case _ => Left(LocalDateParseFailure("The value of element 'DatOfPreMES9' is neither 6 or 8 characters long"))
      }

      dateFormatterEither.flatMap {
        dateFormatter =>
          Try(LocalDate.parse(dateOfPrepString, dateFormatter)) match {
            case Success(value) => Right(value)
            case Failure(_) =>
              Left(LocalDateParseFailure(s"The value of element 'DatOfPreMES9' is not valid with respect to pattern '${dateFormatter.toFormat.toString}'"))
          }
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
        case refString if refString.nonEmpty => Right(refString)
        case _                               => Left(EmptyLocalReferenceNumber("The element 'RefNumHEA4' must contain a value."))
    })

  val mrnR: ReaderT[ParseHandler, NodeSeq, MovementReferenceNumber] =
    ReaderT[ParseHandler, NodeSeq, MovementReferenceNumber](xml =>
      (xml \ "HEAHEA" \ "DocNumHEA5").text match {
        case mrnString if mrnString.nonEmpty => Right(MovementReferenceNumber(mrnString))
        case _                               => Left(EmptyMovementReferenceNumber("The element 'DocNumHEA5' must contain a value."))
    })
}
