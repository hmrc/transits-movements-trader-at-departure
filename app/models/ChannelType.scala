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

package models

sealed abstract class ChannelType(name: String) {
  override def toString: String = name
}

object ChannelType extends Enumerable.Implicits {
  object Web     extends ChannelType("web")
  object Api     extends ChannelType("api")
  object Deleted extends ChannelType("deleted")

  val values: Seq[ChannelType] = Seq(Web, Api)

  implicit val enumerable: Enumerable[ChannelType] =
    Enumerable(values.map(v => v.toString -> v): _*)

}
