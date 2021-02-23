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
import cats.data.NonEmptyList
import cats.data.Reader
import models.MessageType.CancellationDecision
import models.MessageType.ControlDecisionNotification
import models.MessageType.DeclarationCancellationRequest
import models.MessageType.DeclarationRejected
import models.MessageType.DepartureDeclaration
import models.MessageType.GuaranteeNotValid
import models.MessageType.MrnAllocated
import models.MessageType.NoReleaseForTransit
import models.MessageType.ReleaseForTransit
import models.MessagesSummary
import models._

class MessageSummaryService {

  import MessageSummaryService._

  def messagesSummary(departure: Departure): MessagesSummary =
    (for {
      declaration                    <- declarationMessage
      declarationRejection           <- declarationRejectionMessage
      mrnAllocated                   <- mrnAllocatedMessage
      guaranteeNotValid              <- guaranteeNotValidRejectionMessage
      cancellationDecision           <- cancellationDecisionMessage
      declarationCancellationRequest <- declarationCancellationRequestMessage
      noReleaseForTransit            <- noReleaseForTransitMessage
      controlDecision                <- controlDecisionMessage
      releaseForTransit              <- releaseForTransitMessage
      //TODO: Other messages need adding
    } yield {
      MessagesSummary(
        departure = departure,
        declaration = declaration._2,
        declarationRejection = declarationRejection.map(_._2),
        mrnAllocated = mrnAllocated.map(_._2),
        guaranteeNotValid = guaranteeNotValid.map(_._2),
        cancellationDecision = cancellationDecision.map(_._2),
        declarationCancellationRequest = declarationCancellationRequest.map(_._2),
        noReleaseForTransit = noReleaseForTransit.map(_._2),
        releaseForTransit = releaseForTransit.map(_._2)
      )
    }).run(departure)

  private[services] val declarationMessage: Reader[Departure, (Message, MessageId)] =
    Reader[Departure, (Message, MessageId)](_.messagesWithId match {
      case NonEmptyList(declaration, Nil) =>
        declaration

      case NonEmptyList(declaration, _ :: Nil) =>
        declaration

      case NonEmptyList((msg @ MessageWithStatus(_, DepartureDeclaration, _, _, _), id), tail) =>
        // This is a workaround since we cannot infer the type of head
        // to be (MovementMessageWithStatus, MessageId) using @ in the pattern match
        val head: (MessageWithStatus, MessageId) = (msg, id)

        tail
          .foldLeft(NonEmptyList.of(head))({
            case (acc, (m @ MessageWithStatus(_, DepartureDeclaration, _, _, _), mid)) => acc :+ Tuple2(m, mid)
            case (acc, _)                                                              => acc
          })
          .toList
          .maxBy(_._1.messageCorrelationId)

      case NonEmptyList((msg, _), _) =>
        // Unreachable but unprovable
        throw new RuntimeException(
          "Reached an invalid state when summarizing Declaration . " +
            "Expected the first message of the movement to be MovementMessageWithStatus with an DepartureDeclaration, " +
            s"but got ${msg.getClass} that contained a ${msg.messageType.code}"
        )
    })

  private[services] val mrnAllocatedMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val mrnAllocated = getLatestMessageWithoutStatus(departure.messagesWithId)(MrnAllocated)

        val mrnAllocatedCount = mrnAllocated.length

        if (mrnAllocatedCount > 0 && departureDeclarationCount(departure.messages) > 0)
          Some(mrnAllocated.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val declarationRejectionMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val rejectionNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(DeclarationRejected)

        val rejectionNotificationCount = rejectionNotifications.length

        if (rejectionNotificationCount > 0 && departureDeclarationCount(departure.messages) == rejectionNotificationCount)
          Some(rejectionNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val cancellationDecisionMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val cancellationDecisionNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(CancellationDecision)

        val cancellationDecisionNotificationCount = cancellationDecisionNotifications.length

        if (cancellationDecisionNotificationCount > 0 && departureDeclarationCount(departure.messages) > 0)
          Some(cancellationDecisionNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val declarationCancellationRequestMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val declarationCancellationRequestNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(DeclarationCancellationRequest)

        val declarationCancellationRequestNotificationCount = declarationCancellationRequestNotifications.length

        if (declarationCancellationRequestNotificationCount > 0 && departureDeclarationCount(departure.messages) > 0)
          Some(declarationCancellationRequestNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val guaranteeNotValidRejectionMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val guaranteeNotValidNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(GuaranteeNotValid)

        val guaranteeNotValidNotificationCount = guaranteeNotValidNotifications.length

        if (guaranteeNotValidNotificationCount > 0 && departureDeclarationCount(departure.messages) == guaranteeNotValidNotificationCount)
          Some(guaranteeNotValidNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val noReleaseForTransitMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val noReleaseForTransitNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(NoReleaseForTransit)

        val noReleaseForTransitNotificationCount = noReleaseForTransitNotifications.length

        if (noReleaseForTransitNotificationCount > 0 && departureDeclarationCount(departure.messages) > 0)
          Some(noReleaseForTransitNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val controlDecisionMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val controlDecisionNotifications = getLatestMessageWithoutStatus(departure.messagesWithId)(ControlDecisionNotification)

        val controlDecisionNotificationCount = controlDecisionNotifications.length

        if (controlDecisionNotificationCount > 0 && departureDeclarationCount(departure.messages) > 0)
          Some(controlDecisionNotifications.maxBy(_._1.messageCorrelationId))
        else
          None
    }

  private[services] val releaseForTransitMessage: Reader[Departure, Option[(Message, MessageId)]] =
    Reader[Departure, Option[(Message, MessageId)]] {
      departure =>
        val releaseForTransitNotification = getLatestMessageWithoutStatus(departure.messagesWithId)(ReleaseForTransit)

        if (releaseForTransitNotification.nonEmpty && departureDeclarationCount(departure.messages) > 0)
          Some(releaseForTransitNotification.maxBy(_._1.messageCorrelationId))
        else
          None
    }
}

object MessageSummaryService {
  private val departureDeclarationCount: NonEmptyList[Message] => Int = {
    movementMessages =>
      movementMessages.toList.count {
        case MessageWithStatus(_, DepartureDeclaration, _, _, _) => true
        case _                                                   => false
      }
  }

  private val getLatestMessageWithoutStatus: NonEmptyList[(Message, MessageId)] => MessageType => Seq[(MessageWithoutStatus, MessageId)] = {
    messagesWithId => messageType =>
      messagesWithId
        .foldLeft(Seq.empty[(MessageWithoutStatus, MessageId)]) {
          case (acc, (m @ MessageWithoutStatus(_, `messageType`, _, _), mid)) => acc :+ Tuple2(m, mid)
          case (acc, _)                                                       => acc
        }
  }
}
