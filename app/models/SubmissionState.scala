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

trait SubmissionState {
  val reason: String
}

case class SubmissionSuccess(departure: Departure)

trait InternalError extends SubmissionState

trait ExternalError extends SubmissionState

case class DepartureNotFound(reason: String)         extends ExternalError
case class InvalidMessageType(reason: String)        extends ExternalError
case class TransitionError(reason: String)           extends ExternalError
case class XMLMRNError(reason: String)               extends ExternalError
case class SubmissionFailureRejected(reason: String) extends ExternalError

case object SubmissionFailureExternal extends ExternalError {
  override val reason: String = ""
}

case object SubmissionFailureInternal extends InternalError {
  override val reason: String = ""
}

case class FailedToLock(departureId: DepartureId, exception: Throwable) extends InternalError {
  override val reason: String = s"failed to lock departure id: ${departureId.index} with Exception: ${exception.getMessage}"
}

case class FailedToUnlock(departureId: DepartureId, exception: Throwable) extends InternalError {
  override val reason: String = s"failed to unlock departure id: ${departureId.index} with Exception: ${exception.getMessage}"
}

case class DepartureAlreadyLocked(departureId: DepartureId) extends InternalError {
  override val reason: String = s"Lock for departure id: ${departureId.index} already exists"
}
