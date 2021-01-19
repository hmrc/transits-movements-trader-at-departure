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

trait ParseError {
  def message: String
}

object ParseError {
  case class LocalDateParseFailure(message: String)        extends ParseError
  case class LocalTimeParseFailure(message: String)        extends ParseError
  case class InvalidRootNode(message: String)              extends ParseError
  case class EmptyMovementReferenceNumber(message: String) extends ParseError
  case class EmptyLocalReferenceNumber(message: String)    extends ParseError
  case class EmptyNodeSeq(message: String)                 extends ParseError
}
