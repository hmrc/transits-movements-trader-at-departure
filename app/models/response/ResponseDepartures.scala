package models.response


import play.api.libs.json.{Json, OWrites}

case class ResponseDepartures(departures: Seq[ResponseDeparture])

object ResponseDepartures {
  implicit val writes: OWrites[ResponseDepartures] = Json.writes[ResponseDepartures]
}
