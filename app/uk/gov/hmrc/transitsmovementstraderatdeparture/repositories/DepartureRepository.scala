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

package uk.gov.hmrc.transitsmovementstraderatdeparture.repositories

import javax.inject.Inject
import play.modules.reactivemongo.ReactiveMongoApi
import uk.gov.hmrc.transitsmovementstraderatdeparture.models.{Departure, DepartureId, DepartureStatus, Message, MessageStatus}

import scala.concurrent.Future
import scala.util.Try

class DepartureRepository @Inject()(mongo: ReactiveMongoApi) {

  def insert(departure: Departure): Future[Try[Unit]] = ???

  def addNewMessage(departureId: DepartureId, message: Message): Future[Try[Unit]] = ???

  def setDepartureStateAndMessageState(departureId: DepartureId, messageId: Int, departureStatus: DepartureStatus, messageStatus: MessageStatus): Future[Try[Unit]] = ???

  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Try[Unit]] = ???

}
