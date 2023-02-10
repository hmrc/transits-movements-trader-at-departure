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

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq

case class TransitWrapper(xml: NodeSeq) {

  override def toString: String = toXml.toString

  def toXml: Node = {

    val transitWrapperNode: Node =
      <transitRequest>
      </transitRequest>

    Elem(
      transitWrapperNode.prefix,
      transitWrapperNode.label,
      transitWrapperNode.attributes,
      transitWrapperNode.scope,
      transitWrapperNode.child.isEmpty,
      transitWrapperNode.child ++ xml: _*
    )
  }
}
