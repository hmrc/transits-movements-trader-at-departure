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

import play.api.libs.json._

package object models {

  implicit class RichJsObject(obj: JsObject) {

    def map(f: (String, JsValue) => (String, JsValue)): JsObject =
      JsObject(obj.fields.map(f.tupled))

    def filter(f: (String, JsValue) => Boolean): JsObject =
      JsObject(obj.fields.filter(f.tupled))

    def filterNot(f: (String, JsValue) => Boolean): JsObject =
      JsObject(obj.fields.filterNot(f.tupled))

    def filterNulls: JsObject =
      map {
        case (k, v: JsObject) =>
          k -> v.filterNulls
        case (k, v: JsArray) =>
          k -> v.filterNulls
        case (k, v) =>
          k -> v
      }.filterNot(
        (_, v) => v == JsNull || v == Json.obj() || v == JsArray()
      )
  }

  implicit class RichJsArray(arr: JsArray) {

    def map(f: JsValue => JsValue): JsArray =
      JsArray(arr.value.map(f))

    def filter(f: JsValue => Boolean): JsArray =
      JsArray(arr.value.filter(f))

    def filterNot(f: JsValue => Boolean): JsArray =
      JsArray(arr.value.filterNot(f))

    def filterNulls: JsArray =
      map {
        case v: JsObject => v.filterNulls
        case v: JsArray  => v.filterNulls
        case v           => v
      }.filterNot(
        v => v == JsNull || v == Json.obj() || v == JsArray()
      )
  }
}
