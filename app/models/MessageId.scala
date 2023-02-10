/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.mvc.PathBindable
import play.api.libs.json.Json
import play.api.libs.json.Format

case class MessageId(value: Int) {
  def index: Int = value - 1
}

object MessageId {

  def fromMessageIdValue(messageId: Int): Option[MessageId] = if (messageId > 0) Some(new MessageId(messageId)) else None

  implicit val formatMessageId: Format[MessageId] = Json.valueFormat[MessageId]

  implicit def pathBindableMessageId(implicit intBindable: PathBindable[Int]): PathBindable[MessageId] = new PathBindable[MessageId] {

    override def bind(key: String, value: String): Either[String, MessageId] =
      intBindable
        .bind(key, value)
        .fold(
          Left(_),
          fromMessageIdValue(_) match {
            case Some(messageId) => Right(messageId)
            case x               => Left(s"Invalid MessageId. The MessageId must be > 0, instead got $x")
          }
        )

    override def unbind(key: String, messageId: MessageId): String =
      intBindable.unbind(key, messageId.value)
  }

}
