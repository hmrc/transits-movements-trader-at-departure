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

import java.time.LocalDateTime

import cats._
import play.api.libs.json.Json
import play.api.libs.json.Writes

sealed trait DepartureUpdate

object DepartureUpdate {

  implicit val departureUpdateSemigroup: Semigroup[DepartureUpdate] = {
    case (_: DepartureStatusUpdate, x: DepartureStatusUpdate) => x
    case (a: DepartureStatusUpdate, m: MessageStatusUpdate)   => CompoundStatusUpdate(a, m)
    case (_: DepartureStatusUpdate, c: CompoundStatusUpdate)  => c

    case (_: MessageStatusUpdate, x: MessageStatusUpdate)   => x
    case (m: MessageStatusUpdate, a: DepartureStatusUpdate) => CompoundStatusUpdate(a, m)
    case (_: MessageStatusUpdate, c: CompoundStatusUpdate)  => c

    case (_: CompoundStatusUpdate, x: CompoundStatusUpdate)  => x
    case (c: CompoundStatusUpdate, a: DepartureStatusUpdate) => c.copy(departureStatusUpdate = a)
    case (c: CompoundStatusUpdate, m: MessageStatusUpdate)   => c.copy(messageStatusUpdate = m)
  }

  implicit val departureUpdateDepartureModifier: DepartureModifier[DepartureUpdate] = {
    case x: MessageStatusUpdate   => DepartureModifier.toJson(x)
    case x: DepartureStatusUpdate => DepartureModifier.toJson(x)
    case x: CompoundStatusUpdate  => DepartureModifier.toJson(x)
  }
}

final case class MessageStatusUpdate(messageId: MessageId, messageStatus: MessageStatus) extends DepartureUpdate

object MessageStatusUpdate extends MongoDateTimeFormats {
  implicit def departureStateUpdate(implicit writes: Writes[MessageStatus]): DepartureModifier[MessageStatusUpdate] =
    DepartureModifier(
      value =>
        Json.obj(
          "$set" ->
            Json.obj(
              s"messages.${value.messageId.index}.status" -> value.messageStatus,
              "lastUpdated"                               -> LocalDateTime.now.withSecond(0).withNano(0)
            )
      )
    )
}

final case class DepartureStatusUpdate(departureStatus: DepartureStatus) extends DepartureUpdate

object DepartureStatusUpdate extends MongoDateTimeFormats {
  implicit def departureStatusUpdate(implicit writes: Writes[DepartureStatus]): DepartureModifier[DepartureStatusUpdate] =
    value =>
      Json.obj(
        "$set" -> Json.obj(
          "status"      -> value.departureStatus,
          "lastUpdated" -> LocalDateTime.now.withSecond(0).withNano(0)
        ))
}

final case class CompoundStatusUpdate(departureStatusUpdate: DepartureStatusUpdate, messageStatusUpdate: MessageStatusUpdate) extends DepartureUpdate

object CompoundStatusUpdate {
  implicit val departureUpdate: DepartureModifier[CompoundStatusUpdate] =
    csu => DepartureModifier.toJson(csu.departureStatusUpdate) deepMerge DepartureModifier.toJson(csu.messageStatusUpdate)
}
