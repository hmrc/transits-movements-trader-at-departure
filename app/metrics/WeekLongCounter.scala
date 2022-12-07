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

package metrics

import com.codahale.metrics.Counter
import com.codahale.metrics.Counting
import com.codahale.metrics.Meter

import java.time.Clock
import java.time.LocalDate
import java.util.concurrent.atomic.LongAdder
import scala.collection.mutable

class WeekLongCounter(clock: Clock, overallCounter: Counter) extends Meter with Counting {

  // visible for testing purposes
  private[metrics] val perDayCounters = new mutable.OpenHashMap[LocalDate, LongAdder]()

  // visible for testing purposes
  private[metrics] def insertNewDate(newDate: LocalDate): LongAdder =
    // Due to the use of this in a synchronised block, the entry may have already been added, so just get it.
    // We expect to hit this method once a day, so no significant performance hit is expected.
    if (perDayCounters.contains(newDate)) {
      perDayCounters(newDate)
    } else {
      // We use -6 days as we want to remove a week prior, isBefore is the equivalent of a < b, not a <= b
      val latestDateToPrune = newDate.plusDays(-6)
      val toRemove = perDayCounters.filter(
        entry => entry._1.isBefore(latestDateToPrune)
      )
      overallCounter.dec(toRemove.map(_._2.sum()).sum)
      toRemove.keys.foreach(perDayCounters.remove)
      new LongAdder()
    }

  def inc(): Unit = {
    val currentDate = LocalDate.now(clock)
    if (!perDayCounters.contains(currentDate)) synchronized {
      perDayCounters.update(currentDate, insertNewDate(currentDate))
    }
    perDayCounters(currentDate).increment()
    overallCounter.inc()
  }

  override def getCount: Long =
    overallCounter.getCount
}
