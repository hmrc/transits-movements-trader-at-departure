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

import play.api.mvc.PathBindable

final class MessageId private (val index: Int) {
  override def toString: String = s"MessageId($index)"

  override def equals(obj: Any): Boolean = obj match {
    case x: MessageId => x.index == this.index
    case _            => false
  }
}

object MessageId {

  def fromMessageIdValue(messageId: Int): Option[MessageId] = if (messageId > 0) Some(new MessageId(messageId - 1)) else None

  def unapply(arg: MessageId): Some[Int] = Some(arg.index + 1)

  def fromIndex(index: Int): MessageId = new MessageId(index)

  implicit val pathBindableMessageId: PathBindable[MessageId] = new PathBindable[MessageId] {
    override def bind(key: String, value: String): Either[String, MessageId] =
      implicitly[PathBindable[Int]]
        .bind(key, value)
        .fold(
          Left(_),
          fromMessageIdValue _ andThen {
            case Some(messageId) => Right(messageId)
            case x               => Left(s"Invalid MessageId. The MessageId must be > 0, instead got $x")
          }
        )

    override def unbind(key: String, value: MessageId): String = {
      val MessageId(messageId) = value
      messageId.toString
    }
  }

}
