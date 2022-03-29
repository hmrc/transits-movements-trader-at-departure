/*
 * Copyright 2022 HM Revenue & Customs
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

import com.codahale.metrics.MetricRegistry
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import java.time.Clock
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.temporal.ChronoUnit

class WeekLongCounterSpec extends AnyFreeSpec with Matchers {

  "Getting the count under normal time" - {

    Range.inclusive(0, 200) foreach {
      value =>
        s"should return a count of $value when incremented $value times" in {
          val sut = new WeekLongCounter(Clock.systemUTC(), new MetricRegistry().counter("dummy"))
          for (_ <- Range(0, value)) yield sut.inc()
          sut.getCount mustBe value
        }
    }
  }

  "Getting the count over a varying period" - {

    Range.inclusive(0, 30) foreach {
      value =>
        s"when advancing over $value days" - {
          Range.inclusive(0, 10) foreach {
            increment =>
              s"with an increment of $increment per day should return a count of ${math.min(value + 1, 7) * increment}" in {
                // Given this clock
                val clock = new FixedClockWrapper(Clock.fixed(Instant.ofEpochMilli(1000), ZoneId.systemDefault()))

                // and this counter
                val sut = new WeekLongCounter(clock, new MetricRegistry().counter("dummy"))

                val incrementRange = Range(0, increment);

                // when we iterate over the number of days we want to advance
                Range(0, value).foreach {
                  _ =>
                    incrementRange.foreach(
                      _ => sut.inc()
                    )
                    clock.wrappedClock = Clock.fixed(clock.wrappedClock.instant().plus(1, ChronoUnit.DAYS), clock.wrappedClock.getZone)
                }

                // plus the final day
                incrementRange.foreach(
                  _ => sut.inc()
                )

                // then we should get the number of days we incremented on up to a maximum of a week, multiplied by the number of increments per day
                sut.getCount mustBe math.min(value + 1, 7) * increment
              }
          }
        }
    }

    Range.inclusive(0, 60) foreach {
      value =>
        s"when advancing over $value half days" - {
          Range.inclusive(0, 10) foreach {
            increment =>
              // As we have per-day resolution, if we only do one half day in the final day (in this case, if we iterate an
              // even number of times, such as 2), then if we hit over a week, we'll add counts for one half day, but have removed
              // two half days.
              //
              // To think about this, consider if we were discarding the previous day, and incrementing by one each half day. We'd
              // get a count of 1 for the first half day, adding this on for the second half day to get 2. When we go to the next
              // half day and count, that's a new day, so we discard the previous day, meaning that we have a count of 1 again.
              // In our case, we're discarding a day a week back instead - but that means we'll still have a similar issue to above.
              val expectedMaximum = math.min(value + 1, 13 + value % 2) * increment
              s"with an increment of $increment per half day should return a count of $expectedMaximum" in {
                // Given this clock
                val clock = new FixedClockWrapper(Clock.fixed(Instant.ofEpochMilli(1000), ZoneId.systemDefault()))

                // and this counter
                val sut = new WeekLongCounter(clock, new MetricRegistry().counter("dummy"))

                val incrementRange = Range(0, increment);

                // when we iterate over the number of half days we want to advance
                Range(0, value).foreach {
                  _ =>
                    incrementRange.foreach(
                      _ => sut.inc()
                    )
                    clock.wrappedClock = Clock.fixed(clock.wrappedClock.instant().plus(12, ChronoUnit.HOURS), clock.wrappedClock.getZone)
                }

                // plus the final half day
                incrementRange.foreach(
                  _ => sut.inc()
                )

                sut.getCount mustBe expectedMaximum
              }
          }
        }
    }
  }

  "Attempting to create a new date twice returns the same instance the second time" in {
    // Given this clock
    val clock = Clock.fixed(Instant.ofEpochMilli(1000), ZoneId.systemDefault())

    // and given it's associated LocalDate
    val localDate = LocalDate.now(clock)

    // and given this WeekLongCounter
    val sut = new WeekLongCounter(clock, new MetricRegistry().counter("dummy"))

    // and given an increment for this time
    sut.inc()

    // then we should be able to see a counter in the perDayCounters
    val storedAdder = sut.perDayCounters(localDate)

    // when we insert the current date as a new date and get the adder...
    val adder = sut.insertNewDate(localDate)

    // then we should have a counter with a value of 1
    adder.sum() mustBe 1

    // and it should be the counter we originally retrieved
    adder must be theSameInstanceAs storedAdder
  }

}

/** Used for swapping out clocks in testing to test various behaviours.
  *
  * @param wrappedClock The initial [[java.time.Clock]] to use.
  */
private class FixedClockWrapper(var wrappedClock: Clock) extends Clock {

  override def getZone: ZoneId =
    wrappedClock.getZone

  override def withZone(zone: ZoneId): Clock =
    wrappedClock.withZone(zone)

  override def instant(): Instant =
    wrappedClock.instant()
}
