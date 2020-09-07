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

sealed case class TransitionError(reason: String)

sealed trait DepartureStatus {
  def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus]
}

object DepartureStatus extends Enumerable.Implicits with MongoDateTimeFormats {

  case object Initialized extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted => Right(DepartureSubmitted)
      case MessageReceivedEvent.MrnAllocated       => Right(MrnAllocated)
      case MessageReceivedEvent.DepartureRejected  => Right(DepartureRejected)
      case _                                       => Left(TransitionError(s"Failed to transition from Initialized to $messageReceived."))
    }
  }

  case object DepartureSubmitted extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.DepartureSubmitted      => Right(DepartureSubmitted)
      case MessageReceivedEvent.PositiveAcknowledgement => Right(PositiveAcknowledgement)
      case MessageReceivedEvent.DepartureRejected       => Right(DepartureRejected)
      case _                                            => Left(TransitionError(s"Failed to transition from DepartureSubmitted to $messageReceived."))
    }
  }

  case object PositiveAcknowledgement extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.PositiveAcknowledgement => Right(PositiveAcknowledgement)
      case MessageReceivedEvent.MrnAllocated            => Right(MrnAllocated)
      case _                                            => Left(TransitionError(s"Failed to transition from PositiveAcknowledgement to $messageReceived"))
    }
  }

  case object MrnAllocated extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.MrnAllocated                   => Right(MrnAllocated)
      case MessageReceivedEvent.ControlDecisionNotification    => Right(ControlDecisionNotification)
      case MessageReceivedEvent.NoReleaseForTransit            => Right(NoReleaseForTransit)
      case MessageReceivedEvent.ReleaseForTransit              => Right(ReleaseForTransit)
      case MessageReceivedEvent.DeclarationCancellationRequest => Right(DeclarationCancellationRequest)
      case _                                                   => Left(TransitionError(s"Failed to transition from MrnAllocated to $messageReceived."))
    }
  }

  case object DepartureRejected extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.DepartureRejected => Right(DepartureRejected)
      case _                                      => Left(TransitionError(s"Failed to transition from ArrivalRejected to $messageReceived."))
    }
  }

  case object ControlDecisionNotification extends DepartureStatus {
    override def transition(messageRecieved: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageRecieved match {
      case MessageReceivedEvent.ControlDecisionNotification => Right(ControlDecisionNotification)
      case MessageReceivedEvent.NoReleaseForTransit         => Right(NoReleaseForTransit)
      case MessageReceivedEvent.ReleaseForTransit           => Right(ReleaseForTransit)
      case _                                                => Left(TransitionError(s"Failed to transition from ControlDecisionNotification to $messageRecieved"))
    }
  }

  case object NoReleaseForTransit extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.NoReleaseForTransit => Right(NoReleaseForTransit)
      case _                                        => Left(TransitionError(s"Failed to transition from NoReleaseForTransit to $messageReceived"))
    }
  }

  case object ReleaseForTransit extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.ReleaseForTransit              => Right(ReleaseForTransit)
      case MessageReceivedEvent.DeclarationCancellationRequest => Right(DeclarationCancellationRequest)
      case MessageReceivedEvent.CancellationDecision           => Right(CancellationDecision)
      case MessageReceivedEvent.WriteOffNotification           => Right(WriteOffNotification)
      case _                                                   => Left(TransitionError(s"Failed to transition from ReleaseForTransit to $messageReceived"))
    }
  }

  case object DeclarationCancellationRequest extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.DeclarationCancellationRequest => Right(DeclarationCancellationRequest)
      case MessageReceivedEvent.CancellationDecision           => Right(CancellationDecision)
      case _                                                   => Left(TransitionError(s"Failed to transition from DeclarationCancellationRequest to $messageReceived"))
    }
  }

  case object CancellationDecision extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.CancellationDecision => Right(CancellationDecision)
      case MessageReceivedEvent.WriteOffNotification => Right(WriteOffNotification)
      case _                                         => Left(TransitionError(s"Failed to transition from CancellationDecision to $messageReceived"))
    }
  }

  case object WriteOffNotification extends DepartureStatus {
    override def transition(messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
      case MessageReceivedEvent.WriteOffNotification => Right(WriteOffNotification)
      case _                                         => Left(TransitionError(s"Failed to transition from WriteOffNotification to $messageReceived"))
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
    CancellationDecision,
    WriteOffNotification
  )

  implicit val enumerable: Enumerable[DepartureStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
