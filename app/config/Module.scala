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

package config

import com.google.inject.AbstractModule
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticateActionProviderImpl
import controllers.actions.AuthenticateGetOptionalDepartureForWriteActionProvider
import controllers.actions.AuthenticateGetOptionalDepartureForWriteActionProviderImpl
import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import controllers.actions.AuthenticatedGetDepartureForReadActionProviderImpl
import repositories.DepartureIdRepository
import repositories.DepartureRepository

class Module extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[AppConfig]).asEagerSingleton()
    bind(classOf[AuthenticateActionProvider]).to(classOf[AuthenticateActionProviderImpl]).asEagerSingleton()
    bind(classOf[DepartureRepository]).asEagerSingleton()
    bind(classOf[DepartureIdRepository]).asEagerSingleton()
    bind(classOf[AuthenticateGetOptionalDepartureForWriteActionProvider]).to(classOf[AuthenticateGetOptionalDepartureForWriteActionProviderImpl])
    bind(classOf[AuthenticatedGetDepartureForReadActionProvider]).to(classOf[AuthenticatedGetDepartureForReadActionProviderImpl])
  }

}