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

import play.api.libs.json.{Format, JsError, JsNumber, JsResult, JsSuccess, JsValue}
import play.api.mvc.PathBindable

import scala.util.Try

case class DepartureId(index: Int) {

}

object DepartureId {
  implicit val formatsDepartureId: Format[DepartureId] = new Format[DepartureId] {
    override def reads(json: JsValue): JsResult[DepartureId] = json match {
      case JsNumber(number) =>
        Try(number.toInt)
          .map(DepartureId(_))
          .map(JsSuccess(_))
          .getOrElse(JsError("Error in converting JsNumber to an Int"))

      case e =>
        JsError(s"Error in deserialization of Json value to an ArrivalId, expected JsNumber got ${e.getClass}")
    }

    override def writes(o: DepartureId): JsNumber = JsNumber(o.index)
  }

  implicit lazy val pathBindable: PathBindable[DepartureId] = new PathBindable[DepartureId] {
    override def bind(key: String, value: String): Either[String, DepartureId] =
      implicitly[PathBindable[Int]].bind(key, value).right.map(DepartureId(_))

    override def unbind(key: String, value: DepartureId): String =
      value.index.toString
  }

}