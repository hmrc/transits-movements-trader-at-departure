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

package testOnly.models

import models.ChannelType
import models.DepartureId
import play.api.libs.functional.syntax._
import play.api.libs.json._

private[testOnly] class SeedDataParameters(
  val numberOfUsers: Int,
  val movementsPerUser: Int,
  val startDepartureId: DepartureId,
  firstEoriValue: Option[SeedEori] = None,
  channel: Option[ChannelType]
) {

  override def equals(obj: Any): Boolean = obj match {
    case x: SeedDataParameters => (numberOfUsers == x.numberOfUsers) && (movementsPerUser == x.movementsPerUser)
    case _                     => false
  }

  val startEori: SeedEori = firstEoriValue.getOrElse(SeedEori("ZZ", 1, 12))

  val numberOfMovements: Int = numberOfUsers * movementsPerUser

  val channelType: ChannelType = channel.getOrElse(ChannelType.Web)

  private val departureIdIterator: Iterator[DepartureId] =
    (startDepartureId.index to (startDepartureId.index + numberOfMovements)).iterator
      .map(DepartureId(_))

  val seedData: Iterator[(DepartureId, SeedEori)] = {
    val eoriIterator =
      (startEori.suffix to (startEori.suffix + numberOfUsers - 1)).toIterator
        .flatMap {
          eoriSuffix =>
            (1 to movementsPerUser).toIterator
              .map(_ => SeedEori(startEori.prefix, eoriSuffix, startEori.padLength))
        }

    departureIdIterator.zip(eoriIterator)
  }

}

object SeedDataParameters {

  def apply(numberOfUsers: Int,
            movementsPerUser: Int,
            startDepartureId: DepartureId,
            firstEoriValue: Option[SeedEori],
            channel: Option[ChannelType]): SeedDataParameters =
    new SeedDataParameters(numberOfUsers, movementsPerUser, startDepartureId, firstEoriValue, channel)

  implicit val reads: Reads[SeedDataParameters] =
    (
      (__ \ "numberOfUsers").read[Int] and
        (__ \ "movementsPerUser").read[Int] and
        (__ \ "startDepartureId").read[DepartureId] and
        (__ \ "startEori").readNullable[SeedEori] and
        (__ \ "channel").readNullable[ChannelType]
    )(SeedDataParameters.apply _)
}
