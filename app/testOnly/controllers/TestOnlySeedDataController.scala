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

package testOnly.controllers

import play.api.Configuration
import play.api.Logging
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import repositories.DepartureIdRepository
import repositories.DepartureRepository
import testOnly.models.SeedDataParameters
import testOnly.models.SeedDataResponse
import testOnly.services.TestOnlySeedDataService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

class TestOnlySeedDataController @Inject()(
  override val messagesApi: MessagesApi,
  cc: ControllerComponents,
  repository: DepartureRepository,
  idRepository: DepartureIdRepository,
  config: Configuration,
  seedingService: TestOnlySeedDataService
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  /** To start the micro-service locally using the test routes run the following command:
    * sbt run -Dplay.http.router=testOnlyDoNotUseInAppConf.Routes -Dfeature-flags.testOnly.enabled=true
    * Example request
    * {{{
    *   curl --location --request POST 'http://localhost:9480/test-only/transits-movements-trader-at-departure/seedData' \
    *   --header 'Content-Type: application/json' \
    *   --data-raw '{ "startEori": "ZZ000000000021", "numberOfUsers": 10, "startDepartureId": 10, "movementsPerUser": 10, "channel": "api" }'
    * }}}
    */
  def seedData: Action[SeedDataParameters] = Action.async(parse.json[SeedDataParameters]) {
    implicit request =>
      if (featureFlag) {
        repository.getMaxDepartureId.onComplete {
          case Success(value) => logger.info(s"Started seeding Departure data with id ${value.get.index}}")
          case Failure(_)     => logger.error("Unable to retrieve the max arrival id")
        }
        dataInsert(request.body).map {
          _ =>
            val response = SeedDataResponse(request.body)
            repository.getMaxDepartureId.onComplete {
              case Success(value) => logger.info(s"Finished seeding Departure data with id ${value.get.index}}")
              case Failure(_)     => logger.error("Unable to retrieve the max arrival id")
            }
            Ok(Json.toJson(response))
        }
      } else {
        Future.successful(NotImplemented("Feature disabled, could not seed data"))
      }
  }

  private def updateLatestId(): Future[Unit] = repository.getMaxDepartureId.flatMap {
    case Some(value) => idRepository.setLatestId(value.index)
    case None        => Future.failed(new IllegalStateException("No Departure found when retrieving max departure id"))
  }

  private def dataInsert(seedDataParameters: SeedDataParameters): Future[Unit] =
    Future
      .sequence {
        seedingService
          .seedDepartures(seedDataParameters)
          .grouped(50)
          .map(repository.bulkInsert)
      }
      .flatMap(
        _ => updateLatestId()
      )
      .recoverWith {
        case e =>
          updateLatestId().flatMap(
            _ => Future.failed(e)
          )
      }

}
