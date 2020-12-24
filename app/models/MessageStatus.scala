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

import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult.EisSubmissionSuccessful

sealed trait MessageStatus {
  def transition(event: EisSubmissionResult): MessageStatus
}

object MessageStatus extends Enumerable.Implicits {

  case object SubmissionPending extends MessageStatus {
    override def transition(event: EisSubmissionResult): MessageStatus = event match {
      case EisSubmissionSuccessful => SubmissionSucceeded
      case _                       => SubmissionFailed
    }
  }

  case object SubmissionFailed extends MessageStatus {
    override def transition(event: EisSubmissionResult): MessageStatus = event match {
      case EisSubmissionSuccessful => SubmissionSucceeded
      case _                       => SubmissionFailed
    }
  }

  case object SubmissionSucceeded extends MessageStatus {
    override def transition(event: EisSubmissionResult): MessageStatus = this
  }

  val values = Seq(
    SubmissionPending,
    SubmissionFailed,
    SubmissionSucceeded
  )

  implicit val enumerable: Enumerable[MessageStatus] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
