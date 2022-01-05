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

package testOnly.models

import play.api.libs.functional.syntax._
import play.api.libs.json._

private[testOnly] case class SeedEori(prefix: String, suffix: Long, padLength: Int) {

  def format: String = {
    val addPadding = s"%0${padLength}d".format(suffix)
    prefix + addPadding
  }

}

private[testOnly] object SeedEori {

  implicit val read: Reads[SeedEori] =
    (
      __.read[String].map(_.substring(0, 2)) and
        __.read[String]
          .map(_.substring(2))
          .filter(s => """[0-9]{12}""".r.findFirstMatchIn(s).isDefined)
          .map(_.toLong) and
        __.read[String].map(_.substring(2).length)
    )(SeedEori(_, _, _))

  implicit val writes: Writes[SeedEori] = Writes[SeedEori](
    x => {
      val addPadding = s"%0${x.padLength}d".format(x.suffix)

      JsString(x.prefix + addPadding)
    }
  )

}
