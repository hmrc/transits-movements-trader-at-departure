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

abstract class XSDFile(val filePath: String)

object XSDFile {
  object MRNAllocatedXSD                extends XSDFile("/xsd/CC028A.xsd")
  object PositiveAcknowledgementXSD     extends XSDFile("/xsd/CC928A.xsd")
  object DepartureRejectedXSD           extends XSDFile("/xsd/CC016A.xsd")
  object ControlDecisionNotificationXSD extends XSDFile("/xsd/CC060A.xsd")
  object NoReleaseForTransitXSD         extends XSDFile("/xsd/CC051A.xsd")
  object ReleaseForTransitXSD           extends XSDFile("/xsd/CC029A.xsd")
  object CancellationDecisionXSD        extends XSDFile("/xsd/CC009A.xsd")
  object WriteOffNotificationXSD        extends XSDFile("/xsd/CC045A.xsd")
}
