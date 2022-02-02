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

package utils

import cats.data.ReaderT
import models.DepartureId
import models.MessageSender
import models.ParseError
import org.json.XML
import play.api.Logging
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import services.XmlMessageParser.ParseHandler

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.xml._
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

object XMLTransformer extends Logging {

  case class MesSenMES3Failure(message: String) extends ParseError

  def addXmlNode(existingNode: String, key: String, value: String, inputXml: NodeSeq): NodeSeq =
    createRuleTransformer(existingNode, key, value).transform(inputXml.head)

  def updateMesSenMES3(departureId: DepartureId, correlationId: Int): ReaderT[ParseHandler, NodeSeq, NodeSeq] =
    ReaderT[ParseHandler, NodeSeq, NodeSeq] {
      body =>
        if ((body \\ "SynVerNumMES2").nonEmpty) {
          val messageSender: MessageSender = MessageSender(departureId, correlationId)
          Right(XMLTransformer.addXmlNode("SynVerNumMES2", "MesSenMES3", messageSender.toString, body))
        } else {
          logger.error(s"Failed to add MesSenMES3 node with warning: ${MesSenMES3Failure(s"Couldn't find SynVerNumMES2 node")}")
          Left(MesSenMES3Failure(s"Failed to set MesSenMES3 because SynVerNumMES2 is missing"))
        }
    }

  private def createRuleTransformer(existingNode: String, key: String, value: String): RuleTransformer =
    new RuleTransformer(new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if elem.label.equalsIgnoreCase(key) =>
          NodeSeq.Empty
        case elem: Elem if elem.label.equalsIgnoreCase(existingNode) =>
          val newNode = Elem(null, key, Null, TopScope, false, Text(value))
          elem ++ newNode

        case other => other
      }
    })

  def toJson(xml: NodeSeq): JsObject =
    Try(Json.parse(XML.toJSONObject(xml.toString).toString).as[JsObject]) match {
      case Success(data) => data
      case Failure(error) =>
        logger.error(s"Failed to convert xml to json with error: ${error.getMessage}")
        Json.obj()
    }

}
