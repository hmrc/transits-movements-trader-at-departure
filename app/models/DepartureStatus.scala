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

sealed case class TransitionError(reason: String)

sealed trait DepartureStatus extends Product with Serializable

object DepartureStatus extends Enumerable.Implicits with MongoDateTimeFormats {

  case object Initialized extends DepartureStatus

  case object DepartureSubmitted extends DepartureStatus

  case object PositiveAcknowledgement extends DepartureStatus

  case object MrnAllocated extends DepartureStatus

  case object DepartureRejected extends DepartureStatus

  case object ControlDecisionNotification extends DepartureStatus

  case object NoReleaseForTransit extends DepartureStatus

  case object ReleaseForTransit extends DepartureStatus

  case object DeclarationCancellationRequest extends DepartureStatus

  case object CancellationDecision extends DepartureStatus

  case object WriteOffNotification extends DepartureStatus

  case object GuaranteeNotValid extends DepartureStatus

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
    WriteOffNotification,
    GuaranteeNotValid
  )

  implicit val enumerable: Enumerable[DepartureStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
