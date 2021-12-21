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

package models

import cats.data.ReaderT

import scala.xml.NodeSeq

sealed trait MessageType extends IeMetadata {
  def code: String
  def rootNode: String
}

object MessageType extends Enumerable.Implicits {

  case object PositiveAcknowledgement              extends IeMetadata("IE928", "CC928A") with MessageType
  case object DepartureDeclaration                 extends IeMetadata("IE015", "CC015B") with MessageType
  case object MrnAllocated                         extends IeMetadata("IE028", "CC028A") with MessageType
  case object DeclarationRejected                  extends IeMetadata("IE016", "CC016A") with MessageType
  case object ControlDecisionNotification          extends IeMetadata("IE060", "CC060A") with MessageType
  case object NoReleaseForTransit                  extends IeMetadata("IE051", "CC051B") with MessageType
  case object ReleaseForTransit                    extends IeMetadata("IE029", "CC029B") with MessageType
  case object DeclarationCancellationRequest       extends IeMetadata("IE014", "CC014A") with MessageType
  case object CancellationDecision                 extends IeMetadata("IE009", "CC009A") with MessageType
  case object WriteOffNotification                 extends IeMetadata("IE045", "CC045A") with MessageType
  case object GuaranteeNotValid                    extends IeMetadata("IE055", "CC055A") with MessageType
  case object XMLSubmissionNegativeAcknowledgement extends IeMetadata("IE917", "CC917A") with MessageType

  val values: Seq[MessageType] =
    Seq(
      PositiveAcknowledgement,
      DepartureDeclaration,
      MrnAllocated,
      DeclarationRejected,
      ControlDecisionNotification,
      NoReleaseForTransit,
      ReleaseForTransit,
      DeclarationCancellationRequest,
      CancellationDecision,
      WriteOffNotification,
      GuaranteeNotValid,
      XMLSubmissionNegativeAcknowledgement
    )

  def getMessageType: ReaderT[Option, NodeSeq, MessageType] =
    ReaderT[Option, NodeSeq, MessageType] {
      nodeSeq =>
        values.find(_.rootNode == nodeSeq.head.label)
    }

  implicit val ordering: Ordering[MessageType] = (x: MessageType, y: MessageType) => {
    (x, y) match {
      case (DepartureDeclaration, _) => -1

      case (PositiveAcknowledgement, DepartureDeclaration) => 1
      case (PositiveAcknowledgement, _)                    => -1

      case (DeclarationRejected, DepartureDeclaration)    => 1
      case (DeclarationRejected, PositiveAcknowledgement) => 1
      case (DeclarationRejected, _)                       => -1

      case (MrnAllocated, DepartureDeclaration)    => 1
      case (MrnAllocated, PositiveAcknowledgement) => 1
      case (MrnAllocated, DeclarationRejected)     => 1
      case (MrnAllocated, _)                       => -1

      case (GuaranteeNotValid, DepartureDeclaration)    => 1
      case (GuaranteeNotValid, PositiveAcknowledgement) => 1
      case (GuaranteeNotValid, DeclarationRejected)     => 1
      case (GuaranteeNotValid, MrnAllocated)            => 1
      case (GuaranteeNotValid, _)                       => -1

      case (ControlDecisionNotification, DepartureDeclaration)    => 1
      case (ControlDecisionNotification, PositiveAcknowledgement) => 1
      case (ControlDecisionNotification, DeclarationRejected)     => 1
      case (ControlDecisionNotification, MrnAllocated)            => 1
      case (ControlDecisionNotification, GuaranteeNotValid)       => 1
      case (ControlDecisionNotification, _)                       => -1

      case (NoReleaseForTransit, DepartureDeclaration)        => 1
      case (NoReleaseForTransit, PositiveAcknowledgement)     => 1
      case (NoReleaseForTransit, DeclarationRejected)         => 1
      case (NoReleaseForTransit, MrnAllocated)                => 1
      case (NoReleaseForTransit, ControlDecisionNotification) => 1
      case (NoReleaseForTransit, GuaranteeNotValid)           => 1
      case (NoReleaseForTransit, _)                           => -1

      case (ReleaseForTransit, DepartureDeclaration)        => 1
      case (ReleaseForTransit, PositiveAcknowledgement)     => 1
      case (ReleaseForTransit, DeclarationRejected)         => 1
      case (ReleaseForTransit, MrnAllocated)                => 1
      case (ReleaseForTransit, ControlDecisionNotification) => 1
      case (ReleaseForTransit, GuaranteeNotValid)           => 1
      case (ReleaseForTransit, NoReleaseForTransit)         => 1
      case (ReleaseForTransit, _)                           => -1

      case (DeclarationCancellationRequest, DepartureDeclaration)        => 1
      case (DeclarationCancellationRequest, PositiveAcknowledgement)     => 1
      case (DeclarationCancellationRequest, DeclarationRejected)         => 1
      case (DeclarationCancellationRequest, MrnAllocated)                => 1
      case (DeclarationCancellationRequest, ControlDecisionNotification) => 1
      case (DeclarationCancellationRequest, GuaranteeNotValid)           => 1
      case (DeclarationCancellationRequest, NoReleaseForTransit)         => 1
      case (DeclarationCancellationRequest, ReleaseForTransit)           => 1
      case (DeclarationCancellationRequest, _)                           => -1

      case (CancellationDecision, DepartureDeclaration)           => 1
      case (CancellationDecision, PositiveAcknowledgement)        => 1
      case (CancellationDecision, DeclarationRejected)            => 1
      case (CancellationDecision, MrnAllocated)                   => 1
      case (CancellationDecision, ControlDecisionNotification)    => 1
      case (CancellationDecision, GuaranteeNotValid)              => 1
      case (CancellationDecision, NoReleaseForTransit)            => 1
      case (CancellationDecision, ReleaseForTransit)              => 1
      case (CancellationDecision, DeclarationCancellationRequest) => 1
      case (CancellationDecision, _)                              => -1

      case (XMLSubmissionNegativeAcknowledgement, DepartureDeclaration)           => 1
      case (XMLSubmissionNegativeAcknowledgement, DeclarationCancellationRequest) => 1
      case (XMLSubmissionNegativeAcknowledgement, _)                              => -1

      case (WriteOffNotification, _) => 1

      case (_, _) => -1
    }
  }

  implicit val enumerable: Enumerable[MessageType] =
    Enumerable(
      values.map(
        v => v.code -> v
      ): _*
    )
}
