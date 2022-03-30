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

import play.api.libs.json.Json
import play.api.libs.json.OWrites

// TODO: Add integration test suite for selectors
sealed trait DepartureSelector

object DepartureSelector {

  implicit val writes: OWrites[DepartureSelector] =
    OWrites {
      case x: DepartureIdSelector => Json.toJsObject(x)(DepartureIdSelector.writes)
      case x: MessageSelector     => Json.toJsObject(x)(MessageSelector.writes)
    }

}

final case class DepartureIdSelector(departureId: DepartureId) extends DepartureSelector

object DepartureIdSelector {

  implicit val writes: OWrites[DepartureIdSelector] = OWrites(
    departureIdSelector =>
      Json.obj(
        "_id" -> departureIdSelector.departureId
    )
  )
}

final case class MessageSelector(departureId: DepartureId, messageId: MessageId) extends DepartureSelector

object MessageSelector {

  implicit val writes: OWrites[MessageSelector] = OWrites(
    messageSelector =>
      Json.obj(
        "$and" -> Json.arr(
          Json.obj("_id"                                                 -> messageSelector.departureId),
          Json.obj(s"messages.${messageSelector.messageId.index}.status" -> Json.obj("$exists" -> true))
        )
    )
  )
}
