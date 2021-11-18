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

package utils

import java.io.File

import base.SpecBase
import config.AppConfig
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.Configuration
import play.api.Environment
import play.api.Mode
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.Helpers.running
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class MessageTranslationSpec extends SpecBase {

  val appBuilder: GuiceApplicationBuilder =
    baseApplicationBuilder.configure("message-translation-file" -> "TestMessageTranslation.json")

  ".translate" - {

    "must replace any JSON keys that exist the message translation file with their translation" in {

      val app = appBuilder.build()

      running(app) {
        val service = app.injector.instanceOf[MessageTranslation]
        val inputJson = Json.obj(
          "field1" -> 1,
          "foo" -> Json.arr(
            Json.obj("field2" -> 2),
            Json.obj("field2" -> 3)
          )
        )

        val expectedResultJson = Json.obj(
          "Description 1" -> 1,
          "foo" -> Json.arr(
            Json.obj("Description 2" -> 2),
            Json.obj("Description 2" -> 3)
          )
        )

        val result = service.translate(inputJson)

        result mustEqual expectedResultJson
      }
    }

    "must not change any keys that aren't in the message translation file" in {

      val app = appBuilder.build()

      running(app) {
        val service = app.injector.instanceOf[MessageTranslation]
        val inputJson = Json.obj(
          "foo" -> 1,
          "bar" -> Json.arr(
            Json.obj("baz" -> 2),
            Json.obj("baz" -> 3)
          )
        )

        val result = service.translate(inputJson)

        result mustEqual inputJson
      }
    }

    "must not change any JSON values" in {

      val environment = Environment(new File("."), getClass.getClassLoader, Mode.Test)
      val config      = Configuration.load(environment)
      val service     = new MessageTranslation(environment, new AppConfig(config, new ServicesConfig(config)))

      val inputJson = Json.obj(
        "foo" -> "field1",
        "bar" -> Json.arr(
          Json.obj("baz" -> "field2"),
          Json.obj("baz" -> "field2")
        )
      )

      val result = service.translate(inputJson)

      result mustEqual inputJson
    }
  }
}
