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

import models.Departure
import models.request.TadPdfRequest
import utils.Logging

import javax.inject.Inject

class MessageRetrievalService @Inject()(messageSummaryService: MessageSummaryService) extends Logging {

  def getTadRequest(departure: Departure): Option[TadPdfRequest] =
    (for {
      declarationMessage       <- messageSummaryService.declarationMessage.map(_._1)
      releaseForTransitMessage <- messageSummaryService.releaseForTransitMessage.map(_.map(_._1))
    } yield {
      releaseForTransitMessage match {
        case Some(releasedForTransit) => Some(TadPdfRequest(declarationMessage.message, releasedForTransit.message))
        case None =>
          logger.warn(s"[getTadRequest] could not generate the TadPdfRequest because the no releaseForTransit message found")
          None
      }
    }).run(departure)
}
