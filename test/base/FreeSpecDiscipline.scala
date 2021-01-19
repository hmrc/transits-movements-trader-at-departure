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

package base

import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline

trait FreeSpecDiscipline extends Discipline {
  self: AnyFreeSpecLike with Configuration =>
  final def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit config: PropertyCheckConfiguration, prettifier: Prettifier, pos: Position): Unit =
    s"$name" - {
      for ((id, prop) <- ruleSet.all.properties) {
        s"$id" in {
          Checkers.check(prop)(convertConfiguration(config), prettifier, pos)
        }
      }
    }
}
