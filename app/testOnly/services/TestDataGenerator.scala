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

package testOnly.services

import cats.data.NonEmptyList
import models.ChannelType
import models.Departure
import models.DepartureId
import models.MessageId
import models.MessageStatus
import models.MessageWithStatus
import models.MessageType.DepartureDeclaration
import testOnly.models.SeedEori
import java.time.Clock
import java.time.LocalDateTime

import javax.inject.Inject

import scala.util.Random
import scala.xml.Elem
import scala.xml.XML

private[services] class TestDataGenerator @Inject()(clock: Clock) {

  def departureMovement(eori: SeedEori, departureId: DepartureId, channelType: ChannelType): Departure = {

    val dateTime = LocalDateTime.now(clock)

    val referenceNumber = Random.alphanumeric.take(20).mkString

    val xml = TestDataXMLGenerator.departureDeclaration(eori.format, referenceNumber)

    val movementMessage = MessageWithStatus(MessageId(1), dateTime, DepartureDeclaration, xml, MessageStatus.SubmissionSucceeded, 1)

    Departure(
      departureId,
      channelType,
      eori.format,
      None,
      referenceNumber,
      dateTime,
      dateTime,
      2,
      NonEmptyList.one(movementMessage),
      None
    )
  }

}

object TestDataXMLGenerator {

  def departureDeclaration(eori: String, reference: String): Elem =
    XML.loadString(
      s"""
      |<CC015B>
      |  <SynIdeMES1>UNOC</SynIdeMES1>
      |  <SynVerNumMES2>3</SynVerNumMES2>
      |  <MesRecMES6>NCTS</MesRecMES6>
      |  <DatOfPreMES9>20190912</DatOfPreMES9>
      |  <TimOfPreMES10>1222</TimOfPreMES10>
      |  <IntConRefMES11>WE190912102530</IntConRefMES11>
      |  <AppRefMES14>NCTS</AppRefMES14>
      |  <TesIndMES18>0</TesIndMES18>
      |  <MesIdeMES19>1</MesIdeMES19>
      |  <MesTypMES20>GB015B</MesTypMES20>
      |  <HEAHEA>
      |    <RefNumHEA4>$reference</RefNumHEA4>
      |    <TypOfDecHEA24>T2</TypOfDecHEA24>
      |    <CouOfDesCodHEA30>IT</CouOfDesCodHEA30>
      |    <AgrLocOfGooCodHEA38>default</AgrLocOfGooCodHEA38>
      |    <AgrLocOfGooHEA39>default</AgrLocOfGooHEA39>
      |    <AgrLocOfGooHEA39LNG>EN</AgrLocOfGooHEA39LNG>
      |    <AutLocOfGooCodHEA41>default</AutLocOfGooCodHEA41>
      |    <PlaOfLoaCodHEA46>DOVER</PlaOfLoaCodHEA46>
      |    <CouOfDisCodHEA55>GB</CouOfDisCodHEA55>
      |    <CusSubPlaHEA66>default</CusSubPlaHEA66>
      |    <InlTraModHEA75>20</InlTraModHEA75>
      |    <IdeOfMeaOfTraAtDHEA78>EU_EXIT</IdeOfMeaOfTraAtDHEA78>
      |    <IdeOfMeaOfTraAtDHEA78LNG>EN</IdeOfMeaOfTraAtDHEA78LNG>
      |    <IdeOfMeaOfTraCroHEA85>EU_EXIT</IdeOfMeaOfTraCroHEA85>
      |    <IdeOfMeaOfTraCroHEA85LNG>EN</IdeOfMeaOfTraCroHEA85LNG>
      |    <ConIndHEA96>0</ConIndHEA96>
      |    <DiaLanIndAtDepHEA254>EN</DiaLanIndAtDepHEA254>
      |    <NCTSAccDocHEA601LNG>EN</NCTSAccDocHEA601LNG>
      |    <TotNumOfIteHEA305>1</TotNumOfIteHEA305>
      |    <TotNumOfPacHEA306>1</TotNumOfPacHEA306>
      |    <TotGroMasHEA307>1000</TotGroMasHEA307>
      |    <DecDatHEA383>20190912</DecDatHEA383>
      |    <DecPlaHEA394>DOVER</DecPlaHEA394>
      |    <DecPlaHEA394LNG>EN</DecPlaHEA394LNG>
      |  </HEAHEA>
      |  <TRAPRIPC1>
      |    <NamPC17>CITY WATCH SHIPPING</NamPC17>
      |    <StrAndNumPC122>125 Psuedopolis Yard</StrAndNumPC122>
      |    <PosCodPC123>SS99 1AA</PosCodPC123>
      |    <CitPC124>Ank-Morpork</CitPC124>
      |    <CouPC125>GB</CouPC125>
      |    <NADLNGPC>EN</NADLNGPC>
      |    <TINPC159>$eori</TINPC159>
      |  </TRAPRIPC1>
      |  <TRACONCO1>
      |    <NamCO17>QUIRM ENGINEERING</NamCO17>
      |    <StrAndNumCO122>125 Psuedopolis Yard</StrAndNumCO122>
      |    <PosCodCO123>SS99 1AA</PosCodCO123>
      |    <CitCO124>Ank-Morpork</CitCO124>
      |    <CouCO125>GB</CouCO125>
      |    <TINCO159>GB602070107000</TINCO159>
      |  </TRACONCO1>
      |  <TRACONCE1>
      |    <NamCE17>DROFL POTTERY</NamCE17>
      |    <StrAndNumCE122>125 Psuedopolis Yard</StrAndNumCE122>
      |    <PosCodCE123>SS99 1AA</PosCodCE123>
      |    <CitCE124>Ank-Morpork</CitCE124>
      |    <CouCE125>GB</CouCE125>
      |    <NADLNGCE>EN</NADLNGCE>
      |    <TINCE159>GB658120050000</TINCE159>
      |  </TRACONCE1>
      |  <CUSOFFDEPEPT>
      |    <RefNumEPT1>GB000060</RefNumEPT1>
      |  </CUSOFFDEPEPT>
      |  <CUSOFFTRARNS>
      |    <RefNumRNS1>FR001260</RefNumRNS1>
      |    <ArrTimTRACUS085>201909160100</ArrTimTRACUS085>
      |  </CUSOFFTRARNS>
      |  <CUSOFFDESEST>
      |    <RefNumEST1>IT021100</RefNumEST1>
      |  </CUSOFFDESEST>
      |  <SEAINFSLI>
      |    <SeaNumSLI2>1</SeaNumSLI2>
      |    <SEAIDSID>
      |      <SeaIdeSID1>Seal001</SeaIdeSID1>
      |      <SeaIdeSID1LNG>EN</SeaIdeSID1LNG>
      |    </SEAIDSID>
      |  </SEAINFSLI>
      |  <GUAGUA>
      |    <GuaTypGUA1>0</GuaTypGUA1>
      |    <GUAREFREF>
      |      <GuaRefNumGRNREF1>20GB0000010000GX1</GuaRefNumGRNREF1>
      |      <AccCodREF6>test</AccCodREF6>
      |    </GUAREFREF>
      |  </GUAGUA>
      |  <GOOITEGDS>
      |    <IteNumGDS7>1</IteNumGDS7>
      |    <ComCodTarCodGDS10>default</ComCodTarCodGDS10>
      |    <DecTypGDS15>default</DecTypGDS15>
      |    <GooDesGDS23>Flowers</GooDesGDS23>
      |    <GooDesGDS23LNG>EN</GooDesGDS23LNG>
      |    <GroMasGDS46>1000</GroMasGDS46>
      |    <NetMasGDS48>999</NetMasGDS48>
      |    <CouOfDesGDS59>ex</CouOfDesGDS59>
      |    <SPEMENMT2>
      |      <AddInfMT21>20GB0000010000GX1</AddInfMT21>
      |      <AddInfCodMT23>CAL</AddInfCodMT23>
      |    </SPEMENMT2>
      |    <PACGS2>
      |      <MarNumOfPacGS21>Bloomingales</MarNumOfPacGS21>
      |      <MarNumOfPacGS21LNG>EN</MarNumOfPacGS21LNG>
      |      <KinOfPacGS23>BX</KinOfPacGS23>
      |      <NumOfPacGS24>1</NumOfPacGS24>
      |    </PACGS2>
      |  </GOOITEGDS>
      |</CC015B>
      |""".stripMargin
    )
}
