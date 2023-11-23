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

import play.api.libs.json._
import play.api.mvc.PathBindable

sealed abstract class ChannelType(name: String) {
  override def toString: String = name
}

object ChannelType {
  object Web extends ChannelType("web")
  object Api extends ChannelType("api")

  val values: Set[ChannelType] = Set(Web, Api)

  implicit val enumerable: Enumerable[ChannelType] = Enumerable(
    values.toSeq.map(
      v => v.toString -> v
    ): _*
  )

  implicit val formats: Format[ChannelType] = new Format[ChannelType] {

    override def reads(json: JsValue): JsResult[ChannelType] = json match {
      case JsString("web") => JsSuccess(Web)
      case JsString("api") => JsSuccess(Api)
      case _               => JsError("error.invalid")
    }

    override def writes(o: ChannelType): JsValue = JsString(o.toString)
  }

  implicit def pathBindable[ChannelType](implicit ev: Enumerable[ChannelType]): PathBindable[ChannelType] =
    new PathBindable[ChannelType] {

      override def bind(key: String, value: String): Either[String, ChannelType] =
        ev.withName(value).toRight("error.invalid")

      override def unbind(key: String, value: ChannelType): String =
        value.toString
    }
}
