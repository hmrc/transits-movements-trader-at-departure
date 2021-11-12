package audit

import models.ChannelType
import play.api.libs.json.{Json, OWrites}

case class AuthenticationDetails(channel: ChannelType, enrolmentType: String)

object AuthenticationDetails {
  implicit val writes: OWrites[AuthenticationDetails] = (details: AuthenticationDetails) => {
    Json.obj(
      "channel"       -> details.channel,
      "enrolmentType" -> details.enrolmentType
    )
  }
}
