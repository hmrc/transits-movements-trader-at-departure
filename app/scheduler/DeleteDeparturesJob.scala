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

package scheduler

import java.time.temporal.ChronoUnit
import java.time.Instant
import java.time.LocalTime
import java.util.concurrent.TimeUnit

import javax.inject.Inject
import javax.inject.Singleton
import org.quartz.JobExecutionContext
import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier
import workers.DeleteDeparturesWorker

import scala.concurrent.duration._

@Singleton
class DeleteDeparturesJob @Inject()(deleteWorker: DeleteDeparturesWorker) extends ScheduledJob with Logging {
  implicit private val headers: HeaderCarrier = HeaderCarrier()

  override def jobName: String = "Delete Departures"

  override def schedule: Either[Duration, LocalTime] = Left(1.minutes)

  override def execute(context: JobExecutionContext): Unit = {
    logger.info(s"$jobName started at ${Instant.now}")

    deleteWorker.start()
  }
}
