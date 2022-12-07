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

package config

object Constants {
  val BoxName: String = "customs/transits##1.0##notificationUrl"

  val XClientIdHeader: String  = "X-Client-Id"
  val XRequestIdHeader: String = "X-Request-Id"

  val LegacyEnrolmentKey: String   = "HMCE-NCTS-ORG"
  val LegacyEnrolmentIdKey: String = "VATRegNoTURN"

  val NewEnrolmentKey: String   = "HMRC-CTC-ORG"
  val NewEnrolmentIdKey: String = "EORINumber"
}
