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

import javax.inject.Inject
import javax.inject.Singleton
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import models.ChannelType

@Singleton
class AppConfig @Inject()(config: Configuration, servicesConfig: ServicesConfig) {

  val authBaseUrl: String = servicesConfig.baseUrl("auth")

  val auditingEnabled: Boolean = config.get[Boolean]("auditing.enabled")
  val graphiteHost: String     = config.get[String]("microservice.metrics.graphite.host")

  val appName: String = config.get[String]("appName")

  val env: String = config.get[String]("env")

  private val manageDocumentsBaseUrl: String = servicesConfig.baseUrl("manage-documents")
  val manageDocumentsUrl: String             = manageDocumentsBaseUrl ++ config.get[String]("microservice.services.manage-documents.uri")

  private val eisBaseUrl: String = servicesConfig.baseUrl("eis")
  val eisUrl: String             = eisBaseUrl ++ config.get[String]("microservice.services.eis.uri")

  private val pushPullBaseUrl: String = servicesConfig.baseUrl("push-pull-notifications-api")
  val pushPullUrl: String             = pushPullBaseUrl ++ servicesConfig.getConfString("push-pull-notifications-api.uri", "")

  val enrolmentKey: String   = config.get[String]("keys.enrolmentKey")
  val lockRepositoryTtl: Int = config.get[Int]("mongodb.lockRepository.timeToLiveInSeconds")
  val cacheTtl               = config.get[Int]("mongodb.timeToLiveInSeconds")

  val messageTranslationFile: String = config.get[String]("message-translation-file")

  def maxRowsReturned(ct: ChannelType): Int = config.get[Int](s"mongodb.$ct.maxRowsReturned")
}
