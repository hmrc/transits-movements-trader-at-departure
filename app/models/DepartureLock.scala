package models

import play.api.libs.functional.syntax._
import play.api.libs.json.Format
import play.api.libs.json.__

import java.time.LocalDateTime

case class DepartureLock(
  id: String,
  created: LocalDateTime
)

object DepartureLock {

  implicit val format: Format[DepartureLock] = {
    implicit val dtf: Format[LocalDateTime] = MongoDateTimeFormats.localDateTimeFormat
    ((__ \ "_id").format[String]
      ~ (__ \ "created").format[LocalDateTime])(DepartureLock.apply, unlift(DepartureLock.unapply))
  }

  val id      = "_id"
  val created = "created"

}
