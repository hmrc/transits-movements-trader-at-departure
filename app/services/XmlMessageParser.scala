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

import java.time.{LocalDate, LocalDateTime, LocalTime}
import cats.data.ReaderT
import scala.util.Try
import scala.xml.NodeSeq
import cats.implicits._
import models.MessageType
import utils.Format

object XmlMessageParser {

  def correctRootNodeR(messageType: MessageType): ReaderT[Option, NodeSeq, Unit] =
    ReaderT[Option, NodeSeq, Unit] {
      nodeSeq =>
        if (nodeSeq.head.label == messageType.rootNode) Some(()) else None
    }

  val dateOfPrepR: ReaderT[Option, NodeSeq, LocalDate] =
    ReaderT[Option, NodeSeq, LocalDate](xml => {
      (xml \ "DatOfPreMES9").text match {
        case x if x.isEmpty => None
        case x => {
          Try {
            LocalDate.parse(x, Format.dateFormatter)
          }.toOption // TODO: We are not propagating this failure back, do we need to do this?
        }
      }
    })

  val timeOfPrepR: ReaderT[Option, NodeSeq, LocalTime] =
    ReaderT[Option, NodeSeq, LocalTime](xml => {
      (xml \ "TimOfPreMES10").text match {
        case x if x.isEmpty => None
        case x => {
          Try {
            LocalTime.parse(x, Format.timeFormatter)
          }.toOption // TODO: We are not propagating this failure back, do we need to do this?
        }
      }
    })

  val dateTimeOfPrepR: ReaderT[Option, NodeSeq, LocalDateTime] =
    for {
      date <- dateOfPrepR
      time <- timeOfPrepR
    } yield LocalDateTime.of(date, time)

  val referenceR: ReaderT[Option, NodeSeq, String] =
    ReaderT[Option, NodeSeq, String](xml =>
      (xml \ "HEAHEA" \ "RefNumHEA4").text match {
        case refString if !refString.isEmpty => Some(refString)
        case _                               => None
      })
}
