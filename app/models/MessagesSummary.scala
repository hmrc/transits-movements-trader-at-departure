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

import play.api.libs.json._

case class MessagesSummary(
  departure: Departure,
  declaration: MessageId,
  declarationRejection: Option[MessageId] = None,
  mrnAllocated: Option[MessageId] = None,
  guaranteeNotValid: Option[MessageId] = None,
  cancellationDecision: Option[MessageId] = None,
  declarationCancellationRequest: Option[MessageId] = None,
  noReleaseForTransit: Option[MessageId] = None,
  controlDecision: Option[MessageId] = None,
  releaseForTransit: Option[MessageId] = None,
  xmlSubmissionNegativeAcknowledgement: Option[MessageId] = None
)

object MessagesSummary {

  implicit val writes: OWrites[MessagesSummary] =
    OWrites[MessagesSummary] {
      case MessagesSummary(
          departure,
          declaration,
          declarationRejection,
          mrnAllocated,
          guaranteeNotValidId,
          cancellationDecision,
          declarationCancellationRequest,
          noReleaseForTransit,
          controlDecision,
          releaseForTransit,
          xmlSubmissionNegativeAcknowledgement
          ) =>
        Json
          .obj(
            "departureId" -> departure.departureId,
            "messages" -> Json.obj(
              MessageType.DepartureDeclaration.code -> controllers.routes.MessagesController.getMessage(departure.departureId, declaration).url,
              MessageType.DeclarationRejected.code  -> declarationRejection.map(controllers.routes.MessagesController.getMessage(departure.departureId, _).url),
              MessageType.MrnAllocated.code         -> mrnAllocated.map(controllers.routes.MessagesController.getMessage(departure.departureId, _).url),
              MessageType.GuaranteeNotValid.code    -> guaranteeNotValidId.map(controllers.routes.MessagesController.getMessage(departure.departureId, _).url),
              MessageType.CancellationDecision.code -> cancellationDecision.map(controllers.routes.MessagesController.getMessage(departure.departureId, _).url),
              MessageType.DeclarationCancellationRequest.code -> declarationCancellationRequest.map(
                controllers.routes.MessagesController.getMessage(departure.departureId, _).url
              ),
              MessageType.NoReleaseForTransit.code -> noReleaseForTransit.map(controllers.routes.MessagesController.getMessage(departure.departureId, _).url),
              MessageType.ControlDecisionNotification.code -> controlDecision.map(
                controllers.routes.MessagesController.getMessage(departure.departureId, _).url
              ),
              MessageType.ReleaseForTransit.code -> releaseForTransit.map(
                controllers.routes.MessagesController.getMessage(departure.departureId, _).url
              ),
              MessageType.XMLSubmissionNegativeAcknowledgement.code -> xmlSubmissionNegativeAcknowledgement.map(
                controllers.routes.MessagesController.getMessage(departure.departureId, _).url
              )
            )
          )
          .filterNulls
    }
}
