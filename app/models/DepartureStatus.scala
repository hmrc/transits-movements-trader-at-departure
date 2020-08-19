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

object DepartureStatus extends Enumerable.Implicits with MongoDateTimeFormats {

  case object Initialized extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted => DepartureSubmitted
      case MessageReceivedEvent.MrnAllocated       => MrnAllocated
      case MessageReceivedEvent.DepartureRejected  => DepartureRejected
      case _                                       => throw new Exception(s"Tried to transition from Initialized to $messageReceived.")
    }
  }

  case object DepartureSubmitted extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted      => DepartureSubmitted
      case MessageReceivedEvent.PositiveAcknowledgement => PositiveAcknowledgement
      case MessageReceivedEvent.DepartureRejected       => DepartureRejected
      case _                                            => throw new Exception(s"Tried to transition from DepartureSubmitted to $messageReceived.")
    }
  }

  case object PositiveAcknowledgement extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.PositiveAcknowledgement => PositiveAcknowledgement
      case MessageReceivedEvent.MrnAllocated            => MrnAllocated
      case _                                            => throw new Exception(s"Tried to transition from PositiveAcknowledgement to $messageReceived")
    }
  }

  case object MrnAllocated extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.MrnAllocated                   => MrnAllocated
      case MessageReceivedEvent.ControlDecisionNotification    => ControlDecisionNotification
      case MessageReceivedEvent.NoReleaseForTransit            => NoReleaseForTransit
      case MessageReceivedEvent.ReleaseForTransit              => ReleaseForTransit
      case MessageReceivedEvent.DeclarationCancellationRequest => DeclarationCancellationRequest
      case _                                                   => throw new Exception(s"Tried to transition from MrnAllocated to $messageReceived.")
    }
  }

  case object DepartureRejected extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DepartureRejected => DepartureRejected
      case _                                      => throw new Exception(s"Tried to transition from ArrivalRejected to $messageReceived.")
    }
  }

  case object ControlDecisionNotification extends DepartureStatus {
    override def transition(messageRecieved: MessageReceivedEvent): DepartureStatus = messageRecieved match {
      case MessageReceivedEvent.ControlDecisionNotification => ControlDecisionNotification
      case MessageReceivedEvent.NoReleaseForTransit         => NoReleaseForTransit
      case MessageReceivedEvent.ReleaseForTransit           => ReleaseForTransit
      case _                                                => throw new Exception(s"Tried to transition from ControlDecisionNotification to $messageRecieved")
    }
  }

  case object NoReleaseForTransit extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.NoReleaseForTransit => NoReleaseForTransit
      case _                                        => throw new Exception(s"Tried to transition from NoReleaseForTransit to $messageReceived")
    }
  }

  case object ReleaseForTransit extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.ReleaseForTransit              => ReleaseForTransit
      case MessageReceivedEvent.DeclarationCancellationRequest => DeclarationCancellationRequest
      case MessageReceivedEvent.CancellationDecision           => CancellationDecision
      case _                                                   => throw new Exception(s"Tried to transition from ReleaseForTransit to $messageReceived")
    }
  }

  case object DeclarationCancellationRequest extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.DeclarationCancellationRequest => DeclarationCancellationRequest
      case MessageReceivedEvent.CancellationDecision           => CancellationDecision
      case _                                                   => throw new Exception(s"Tried to transition from DeclarationCancellationRequest to $messageReceived")
    }
  }

  case object CancellationDecision extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): DepartureStatus = messageReceived match {
      case MessageReceivedEvent.CancellationDecision => CancellationDecision
      case _                                         => throw new Exception(s"Tried to transition from CancellationDecision to $messageReceived")
    }
  }

  val values = Seq(
    Initialized,
    DepartureSubmitted,
    PositiveAcknowledgement,
    MrnAllocated,
    DepartureRejected,
    ControlDecisionNotification,
    NoReleaseForTransit,
    ReleaseForTransit,
    DeclarationCancellationRequest,
    CancellationDecision
  )

  implicit val enumerable: Enumerable[DepartureStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
