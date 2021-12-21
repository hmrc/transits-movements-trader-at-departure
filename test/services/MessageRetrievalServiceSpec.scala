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

package services

import base.SpecBase
import cats.data.NonEmptyList
import cats.data.Reader
import models.ChannelType
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.Message
import models.MessageId
import models.MessageType
import models.MessageWithoutStatus
import org.mockito.Mockito.when
import java.time.LocalDateTime
import utils.JsonHelper

class MessageRetrievalServiceSpec extends SpecBase with JsonHelper {

  class Setup {
    val mockMessageSummaryService: MessageSummaryService = mock[MessageSummaryService]
    val service                                          = new MessageRetrievalService(mockMessageSummaryService)
  }

  "MessageRetrievalService" - {
    "getTadRequest" - {
      "Return a Tad Request if both the declaration and releaseFromTransit messages are present" in new Setup {
        val departure: Departure = Departure(
          DepartureId(1),
          ChannelType.Web,
          "AB123456C",
          None,
          "SomeReference",
          LocalDateTime.now(),
          LocalDateTime.now(),
          3,
          NonEmptyList(
            MessageWithoutStatus(
              MessageId(1),
              LocalDateTime.now(),
              MessageType.DepartureDeclaration,
              <departure></departure>,
              1,
              convertXmlToJson(<departure></departure>.toString)
            ),
            List(
              MessageWithoutStatus(
                MessageId(2),
                LocalDateTime.now(),
                MessageType.ReleaseForTransit,
                <released></released>,
                2,
                convertXmlToJson(<released></released>.toString)
              )
            )
          ),
          None
        )

        val date = LocalDateTime.now()

        when(mockMessageSummaryService.releaseForTransitMessage)
          .thenReturn(
            Reader[Departure, Option[(Message, MessageId)]](
              _ =>
                Some(
                  (
                    MessageWithoutStatus(MessageId(3),
                                         date,
                                         MessageType.ReleaseForTransit,
                                         <released></released>,
                                         2,
                                         convertXmlToJson(<released></released>.toString)),
                    MessageId(3)
                  )
              )
            )
          )

        service
          .getReleaseForTransitMessage(departure)
          .value mustBe MessageWithoutStatus(MessageId(3),
                                             date,
                                             MessageType.ReleaseForTransit,
                                             <released></released>,
                                             2,
                                             convertXmlToJson(<released></released>.toString))
      }
      "Return a None if releaseFromTransit message is not present" in new Setup {
        val departure: Departure = Departure(
          DepartureId(1),
          ChannelType.Web,
          "AB123456C",
          None,
          "SomeReference",
          LocalDateTime.now(),
          LocalDateTime.now(),
          3,
          NonEmptyList(
            MessageWithoutStatus(
              MessageId(1),
              LocalDateTime.now(),
              MessageType.DepartureDeclaration,
              <departure></departure>,
              1,
              convertXmlToJson(<departure></departure>.toString)
            ),
            Nil
          ),
          None
        )

        when(mockMessageSummaryService.releaseForTransitMessage)
          .thenReturn(
            Reader[Departure, Option[(Message, MessageId)]](
              _ => None
            )
          )

        service.getReleaseForTransitMessage(departure) mustBe None
      }
    }
  }
}
