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

package controllers.actions

import models.request.DepartureWithMessagesRequest
import models.ChannelType.Web
import models.request.DepartureRequest
import models.Departure
import models.DepartureId
import org.scalatest.exceptions.TestFailedException
import play.api.mvc._
import play.api.test.Helpers.stubBodyParser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider(departure: Departure) extends AuthenticatedGetDepartureWithMessagesForReadActionProvider {

  override def apply(departureId: DepartureId): ActionBuilder[DepartureWithMessagesRequest, AnyContent] =
    new ActionBuilder[DepartureWithMessagesRequest, AnyContent] {
      override def parser: BodyParser[AnyContent] = stubBodyParser()

      override def invokeBlock[A](request: Request[A], block: DepartureWithMessagesRequest[A] => Future[Result]): Future[Result] =
        if (departure.departureId == departureId) {
          block(DepartureWithMessagesRequest(request, departure, Web))
        } else {
          throw new TestFailedException(
            s"Bad test data setup. DepartureId on the Departure was ${departure.departureId} but expected to retrieve $departureId",
            0
          )
        }

      override protected def executionContext: ExecutionContext = implicitly[ExecutionContext]

    }
}

object FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider {

  def apply(departure: Departure): FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider =
    new FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider(departure)
}
