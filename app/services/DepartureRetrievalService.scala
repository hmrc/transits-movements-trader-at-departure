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

package services

import audit.AuditService
import cats.data.EitherT
import models.ChannelType.Deleted
import models.Departure
import models.DepartureId
import models.DepartureNotFound
import models.ErrorState
import models.MessageResponse
import repositories.DepartureRepository
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

private[services] class DepartureRetrievalService @Inject()(repository: DepartureRepository, auditService: AuditService)(implicit ec: ExecutionContext) {

  private def getDepartureById(departureId: DepartureId): Future[Either[ErrorState, Departure]] =
    repository.get(departureId).map {
      case Some(departure) => Right(departure)
      case None            => Left(DepartureNotFound(s"[GetDepartureService][getDepartureById] Unable to retrieve departure message for arrival id: ${departureId.index}"))
    }

  def getDepartureAndAuditDeletedDepartures(departureId: DepartureId, messageResponse: MessageResponse, xml: NodeSeq)(
    implicit hc: HeaderCarrier): EitherT[Future, ErrorState, Departure] =
    EitherT(
      getDepartureById(departureId)
    ).leftMap {
      case submissionState: DepartureNotFound =>
        auditService.auditNCTSMessages(Deleted, "Deleted", messageResponse, xml)
        submissionState
      case submissionState => submissionState
    }
}
