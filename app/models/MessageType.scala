/*
 * Copyright 2022 HM Revenue & Customs
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
  def id: Int
  def code: String
  def rootNode: String
}

object MessageType extends Enumerable.Implicits {

  case object DepartureDeclaration                 extends IeMetadata(1, "IE015", "CC015B") with MessageType
  case object XMLSubmissionNegativeAcknowledgement extends IeMetadata(2, "IE917", "CC917A") with MessageType
  case object PositiveAcknowledgement              extends IeMetadata(3, "IE928", "CC928A") with MessageType
  case object DeclarationRejected                  extends IeMetadata(4, "IE016", "CC016A") with MessageType
  case object MrnAllocated                         extends IeMetadata(5, "IE028", "CC028A") with MessageType
  case object GuaranteeNotValid                    extends IeMetadata(6, "IE055", "CC055A") with MessageType
  case object ControlDecisionNotification          extends IeMetadata(7, "IE060", "CC060A") with MessageType
  case object NoReleaseForTransit                  extends IeMetadata(8, "IE051", "CC051B") with MessageType
  case object ReleaseForTransit                    extends IeMetadata(9, "IE029", "CC029B") with MessageType
  case object DeclarationCancellationRequest       extends IeMetadata(10, "IE014", "CC014A") with MessageType
  case object CancellationDecision                 extends IeMetadata(11, "IE009", "CC009A") with MessageType
  case object WriteOffNotification                 extends IeMetadata(12, "IE045", "CC045A") with MessageType

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

  // ensures XMLSubmissionNegativeAcknowledgement is sorted after DeclarationCancellationRequest
  implicit val latestMessageOrdering: Ordering[MessageType] = (x: MessageType, y: MessageType) => {
    (x, y) match {
      case (XMLSubmissionNegativeAcknowledgement, DeclarationCancellationRequest) => 1
      case (DeclarationCancellationRequest, XMLSubmissionNegativeAcknowledgement) => -1

      case (_, _) => x.id.compareTo(y.id)
    }
  }

  // ensures DeclarationCancellationRequest and DepartureDeclaration are sorted after everything else
  val previousMessageOrdering: Ordering[MessageType] = (x: MessageType, y: MessageType) => {
    (x, y) match {
      case (DeclarationCancellationRequest, _) => 1
      case (_, DeclarationCancellationRequest) => -1

      case (DepartureDeclaration, _) => 1
      case (_, DepartureDeclaration) => -1

      case (_, _) => x.id.compareTo(y.id)
    }
  }

  implicit val enumerable: Enumerable[MessageType] =
    Enumerable(
      values.map(
        v => v.code -> v
      ): _*
    )
}
