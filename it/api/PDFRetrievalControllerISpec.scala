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

package api

import api.helpers.ApiSpecBase
import cats.data.NonEmptyList
import models.ChannelType
import models.Departure
import models.DepartureId
import models.DepartureStatus
import models.MessageId
import models.MessageStatus
import models.MessageType
import models.MessageWithStatus
import models.MessageWithoutStatus
import play.api.Application
import play.api.http.ContentTypes
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.test.Helpers.BAD_GATEWAY
import play.api.test.Helpers.OK
import repositories.DepartureRepository
import utils.JsonHelper

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDateTime
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class PDFRetrievalControllerISpec extends ApiSpecBase with JsonHelper {

  implicit override lazy val app: Application = appBuilder.build()

  lazy val path: Path                    = Paths.get(getClass.getResource("/test-files/testPDF.pdf").toURI)
  lazy val pdfFile: Array[Byte]          = Files.readAllBytes(path)
  val wsClient: WSClient                 = app.injector.instanceOf[WSClient]
  val departureRepo: DepartureRepository = app.injector.instanceOf[DepartureRepository]

  override protected def portConfigKeys: Seq[String] = Seq(
    "microservice.services.auth.port",
    "microservice.services.manage-documents.port"
  )

  "/movements/departures/:departureId/accompanying-document" - {
    "should return a Tsad PDF if all data is present" in {

      val requestId: String = UUID.randomUUID().toString
      val clientId: String = UUID.randomUUID().toString

      val departure: Departure = Departure(
        DepartureId(12),
        ChannelType.Web,
        "1234567",
        None,
        "SomeReference",
        LocalDateTime.now(),
        LocalDateTime.now(),
        3,
        NonEmptyList(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.now(),
            MessageType.DepartureDeclaration,
            <departure></departure>,
            MessageStatus.SubmissionSucceeded,
            1,
            convertXmlToJson(<departure></departure>.toString)
          ),
          List(
            MessageWithoutStatus(
              MessageId(2),
              LocalDateTime.now(),
              MessageType.ReleaseForTransit,
              <released><HEAHEA><SecHEA358>1</SecHEA358></HEAHEA></released>,
              2,
              convertXmlToJson(<released><HEAHEA><SecHEA358>1</SecHEA358></HEAHEA></released>.toString)
            )
          )
        ),
        None
      )

      stubForPostWithResponseBody(
        url = "/auth/authorise",
        body = """{
          | "authorisedEnrolments": [{
          |   "key": "HMCE-NCTS-ORG",
          |   "identifiers": [{
          |     "key": "VatRegNoTURN",
          |     "value": "1234567"
          |   }],
          |   "state": "Activated"
          | }]
          |}""".stripMargin.getBytes(),
        requestId = requestId
      )

      database.flatMap(_.drop()).futureValue
      departureRepo.insert(departure).futureValue
      departureRepo.get(departure.departureId).futureValue mustBe Some(departure)

      stubForPostWithResponseBody(
        url = "/transit-movements-trader-manage-documents/transit-security-accompanying-document",
        body = pdfFile,
        requestId = requestId,
        extraHeaders = Seq(HeaderNames.CONTENT_TYPE -> ContentTypes.XML, HeaderNames.USER_AGENT -> "transits-movements-trader-at-departure", "X-Client-Id" -> clientId)
      )

      val response = wsClient
        .url(s"http://localhost:$port/transits-movements-trader-at-departure/movements/departures/12/accompanying-document")
        .withHttpHeaders("channel" -> "web", "X-Client-Id" -> clientId, "X-Request-ID" -> requestId)
        .get()
        .futureValue

      response.status mustBe OK
      response.bodyAsBytes mustBe pdfFile
    }

    "should return a Tad PDF if all data is present" in {

      val requestId: String = UUID.randomUUID().toString
      val clientId: String = UUID.randomUUID().toString

      val departure: Departure = Departure(
        DepartureId(12),
        ChannelType.Web,
        "1234567",
        None,
        "SomeReference",
        LocalDateTime.now(),
        LocalDateTime.now(),
        3,
        NonEmptyList(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.now(),
            MessageType.DepartureDeclaration,
            <departure></departure>,
            MessageStatus.SubmissionSucceeded,
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

      stubForPostWithResponseBody(
        url = "/auth/authorise",
        body = """{
          | "authorisedEnrolments": [{
          |   "key": "HMCE-NCTS-ORG",
          |   "identifiers": [{
          |     "key": "VatRegNoTURN",
          |     "value": "1234567"
          |   }],
          |   "state": "Activated"
          | }]
          |}""".stripMargin.getBytes(),
        requestId = requestId
      )

      database.flatMap(_.drop()).futureValue
      departureRepo.insert(departure).futureValue
      departureRepo.get(departure.departureId).futureValue mustBe Some(departure)

      stubForPostWithResponseBody(
        url = "/transit-movements-trader-manage-documents/transit-accompanying-document",
        body = pdfFile,
        requestId = requestId,
        extraHeaders = Seq(HeaderNames.CONTENT_TYPE -> ContentTypes.XML, HeaderNames.USER_AGENT -> "transits-movements-trader-at-departure", "X-Client-Id" -> clientId)
      )

      val response = wsClient
        .url(s"http://localhost:$port/transits-movements-trader-at-departure/movements/departures/12/accompanying-document")
        .withHttpHeaders("channel" -> "web", "X-Client-Id" -> clientId, "X-Request-ID" -> requestId)
        .get()
        .futureValue

      response.status mustBe OK
      response.bodyAsBytes mustBe pdfFile
    }

    "should return a BAD_GATEWAY if the call to the manage document service returns and unexpected result" in {

      val requestId: String = UUID.randomUUID().toString
      val clientId: String = UUID.randomUUID().toString

      val departure: Departure = Departure(
        DepartureId(12),
        ChannelType.Web,
        "1234567",
        None,
        "SomeReference",
        LocalDateTime.now(),
        LocalDateTime.now(),
        3,
        NonEmptyList(
          MessageWithStatus(
            MessageId(1),
            LocalDateTime.now(),
            MessageType.DepartureDeclaration,
            <departure></departure>,
            MessageStatus.SubmissionSucceeded,
            1,
            Json.obj()
          ),
          List(MessageWithoutStatus(MessageId(2), LocalDateTime.now(), MessageType.ReleaseForTransit, <released></released>, 2, Json.obj()))
        ),
        None
      )

      stubForPostWithResponseBody(
        url = "/auth/authorise",
        body = """{
          | "authorisedEnrolments": [{
          |   "key": "HMCE-NCTS-ORG",
          |   "identifiers": [{
          |     "key": "VatRegNoTURN",
          |     "value": "1234567"
          |   }],
          |   "state": "Activated"
          | }]
          |}""".stripMargin.getBytes(),
        requestId = requestId
      )

      database.flatMap(_.drop()).futureValue
      departureRepo.insert(departure).futureValue
      departureRepo.get(departure.departureId).futureValue mustBe Some(departure)

      stubForPostWithResponseBody(
        url = "/transit-movements-trader-manage-documents/transit-accompanying-document",
        body = "Error Something Went Wrong".getBytes(),
        requestId = requestId,
        extraHeaders = Seq(HeaderNames.CONTENT_TYPE -> ContentTypes.XML, HeaderNames.USER_AGENT -> "transits-movements-trader-at-departure", "X-Client-Id" -> clientId),
        status = 500
      )

      val response = wsClient
        .url(s"http://localhost:$port/transits-movements-trader-at-departure/movements/departures/12/accompanying-document")
        .withHttpHeaders("channel" -> "web", "X-Client-Id" -> clientId, "X-Request-ID" -> requestId)
        .get()
        .futureValue

      response.status mustBe BAD_GATEWAY
      response.bodyAsBytes mustBe Array.empty[Byte]
    }
  }
}
