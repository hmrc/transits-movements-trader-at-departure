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

package models

sealed trait DepartureStatus {
  def transition(messageReceived: MessageReceivedEvent): DepartureStatus
}

object DepartureStatus extends Enumerable.Implicits {

  case object Initialized extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted  => DepartureSubmitted
      case MessageReceivedEvent.DepartureAccepted   => DepartureAccepted
      case _                                        => throw new Exception(s"Tried to transition from Initialized to $messageReceived.")
    }
  }

  case object DepartureSubmitted extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted    => DepartureSubmitted
      case MessageReceivedEvent.DepartureAccepted     => DepartureAccepted
      case _                                          => throw new Exception(s"Tried to transition from DepartureSubmitted to $messageReceived.")
    }
  }

  case object DepartureAccepted extends DepartureStatus {
    override def transition(messageRecieved: MessageReceivedEvent): DepartureStatus = messageRecieved match {
      case MessageReceivedEvent.DepartureAccepted => DepartureAccepted
      case _                                      => throw new Exception(s"Tried to transition from DepartureAccepted to $messageRecieved.")
    }
  }

  val values = Seq(
    Initialized,
    DepartureSubmitted,
    DepartureAccepted
  )

  implicit val enumerable: Enumerable[DepartureStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)
}