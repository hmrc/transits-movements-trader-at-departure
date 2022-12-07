/*
 * Copyright 2023 HM Revenue & Customs
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

sealed trait ErrorState {
  val reason: String
}

sealed trait ExternalError                                                            extends ErrorState
sealed abstract class InternalError(val reason: String)                               extends ErrorState
sealed abstract class InternalException(val reason: String, val exception: Throwable) extends ErrorState

object ExternalError {

  def unapply(errorState: ErrorState): Option[String] =
    errorState match {
      case x: ExternalError => Some(x.reason)
      case _                => None
    }
}

object InternalError {

  def unapply(errorState: ErrorState): Option[String] =
    errorState match {
      case x: InternalError => Some(x.reason)
      case _                => None
    }
}

object InternalException {

  def unapply(errorState: ErrorState): Option[(String, Throwable)] =
    errorState match {
      case x: InternalException => Some((x.reason, x.exception))
      case _                    => None
    }
}

final case class DepartureNotFound(reason: String)         extends ExternalError
final case class InvalidMessageType(reason: String)        extends ExternalError
final case class TransitionError(reason: String)           extends ExternalError
final case class XMLMRNError(reason: String)               extends ExternalError
final case class SubmissionFailureRejected(reason: String) extends ExternalError

case object SubmissionFailureExternal extends ExternalError {
  override val reason: String = ""
}

final case class DepartureAlreadyLocked(departureId: DepartureId)
    extends InternalError(
      s"Internal Submission Failure, Lock for departure id: ${departureId.index} already exists"
    )

case object SubmissionFailureInternal extends InternalError("Internal Submission Failure, SubmissionFailureInternal")

final class FailedToLock(departureId: DepartureId, exception: Throwable)
    extends InternalException(
      s"Internal Submission Failure, failed to lock departure id: ${departureId.index} with exception",
      exception
    )

final class FailedToUnlock(departureId: DepartureId, exception: Throwable)
    extends InternalException(
      s"Internal Submission Failure, failed to unlock departure id: ${departureId.index} with exception",
      exception
    )
