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

package controllers.actions

import audit.AuditService
import audit.AuditType.SuccessfulAuthTracking
import cats.data.Ior
import com.kenshoo.play.metrics.Metrics
import config.Constants._
import models.{ChannelType, EORINumber, TURN}
import models.request.AuthenticatedRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest
import play.api.mvc.Results.Forbidden
import play.api.mvc.Results.Unauthorized
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import javax.inject.Inject
import metrics.HasMetrics

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class AuthenticateAction @Inject()(override val authConnector: AuthConnector, val metrics: Metrics, auditService: AuditService)(implicit val executionContext: ExecutionContext, val headerCarrier: HeaderCarrier)
    extends ActionRefiner[Request, AuthenticatedRequest]
    with AuthorisedFunctions
    with Logging
    with HasMetrics {

  def getEnrolmentIdentifier(
    enrolments: Enrolments,
    enrolmentKey: String,
    enrolmentIdKey: String
  ): Option[String] =
    for {
      enrolment  <- enrolments.getEnrolment(enrolmentKey)
      identifier <- enrolment.getIdentifier(enrolmentIdKey)
    } yield identifier.value

  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    ChannelUtil.getChannel(request) match {
      case None =>
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)

        authorised(Enrolment(NewEnrolmentKey) or Enrolment(LegacyEnrolmentKey)).retrieve(Retrievals.authorisedEnrolments) {
          enrolments =>
            val legacyEnrolmentId = getEnrolmentIdentifier(
              enrolments,
              LegacyEnrolmentKey,
              LegacyEnrolmentIdKey
            ).map(TURN.apply)

            val newEnrolmentId = getEnrolmentIdentifier(
              enrolments,
              NewEnrolmentKey,
              NewEnrolmentIdKey
            ).map(EORINumber.apply)

            Ior
              .fromOptions(legacyEnrolmentId, newEnrolmentId)
              .map {
                enrolmentId => {
                  track(channel, enrolmentId)
                  Future.successful(Right(AuthenticatedRequest(request, channel, enrolmentId)))
                }
              }
              .getOrElse {
                Future.failed(InsufficientEnrolments(s"Unable to retrieve enrolment for either $NewEnrolmentKey or $LegacyEnrolmentKey"))
              }
        }
    }
  }.recover {
    case e: InsufficientEnrolments =>
      logger.warn(s"Failed to authorise due to insufficient enrolments", e)
      Left(Forbidden)
    case e: AuthorisationException =>
      logger.warn(s"Failed to authorise", e)
      Left(Unauthorized)
  }

  private def track(channel: ChannelType, enrolmentId: Ior[TURN, EORINumber]) = {

    val enrolmentType = enrolmentId.fold(
      _ => "Legacy",
      _ => "Modern",
      (_, modern) => modern.value
    )
    val message = s"Auth Successful: $channel:$enrolmentType"

    logger.info(message)
    auditService.simpleAudit(SuccessfulAuthTracking, message)
    counter(s"auth-$channel-$enrolmentType")
  }
}
