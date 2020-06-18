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

package uk.gov.hmrc.transitsmovementstraderatdeparture.models

import play.api.mvc.PathBindable

import scala.util.Try

final case class MessageSender(departureId: DepartureId, messageCorrelationId: Int) {
  override val toString: String = {
    s"MDTP-${pad(departureId.index, 27)}-${pad(messageCorrelationId, 2)}"
  }

  private def pad(value: Int, length: Int): String =
    s"%0${length}d".format(value)
}

object MessageSender {

  private val pattern = "MDTP-(\\d+)-(\\d+)".r.anchored

  def apply(value: String): Option[MessageSender] =
    value match {
      case pattern(id, messageCorrelationId) =>
        (for {
          id            <- Try(id.toInt)
          correlationId <- Try(messageCorrelationId.toInt)
        } yield {
          MessageSender(DepartureId(id), correlationId)
        }).toOption
      case _ => None
    }

  implicit lazy val pathBindable: PathBindable[MessageSender] = new PathBindable[MessageSender] {
    override def bind(key: String, value: String): Either[String, MessageSender] =
      apply(value) match {
        case Some(messageSender) => Right(messageSender)
        case None                => Left("Invalid message sender")
      }

    override def unbind(key: String, value: MessageSender): String =
      value.toString
  }
}