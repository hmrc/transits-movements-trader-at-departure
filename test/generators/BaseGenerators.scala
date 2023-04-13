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

package generators

import cats.data.NonEmptyList
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.alphaNumChar
import org.scalacheck.Gen.alphaStr
import org.scalacheck.Gen.choose
import org.scalacheck.Gen.chooseNum
import org.scalacheck.Gen.listOfN
import org.scalacheck.Gen.numChar
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink

trait BaseGenerators {

  implicit def dontShrink[A]: Shrink[A] = Shrink.shrinkAny

  def genIntersperseString(gen: Gen[String], value: String, frequencyV: Int = 1, frequencyN: Int = 10): Gen[String] = {

    val genValue: Gen[Option[String]] = Gen.frequency(frequencyN -> None, frequencyV -> Gen.const(Some(value)))

    for {
      seq1 <- gen
      seq2 <- Gen.listOfN(seq1.length, genValue)
    } yield
      seq1.toSeq.zip(seq2).foldRight("") {
        case ((n, Some(v)), m) =>
          m + n + v
        case ((n, _), m) =>
          m + n
      }
  }

  def intsInRangeWithCommas(min: Int, max: Int): Gen[String] = {
    val numberGen = choose[Int](min, max)
    genIntersperseString(numberGen.toString, ",")
  }

  def intsLargerThanMaxValue: Gen[BigInt] =
    arbitrary[BigInt] suchThat (
      x => x > Int.MaxValue
    )

  def intsSmallerThanMinValue: Gen[BigInt] =
    arbitrary[BigInt] suchThat (
      x => x < Int.MinValue
    )

  def nonNumerics: Gen[String] =
    alphaStr suchThat (_.nonEmpty)

  def decimals: Gen[String] =
    arbitrary[BigDecimal]
      .suchThat(_.abs < Int.MaxValue)
      .suchThat(!_.isValidInt)
      .map(_.formatted("%f"))

  def intsBelowValue(value: Int): Gen[Int] =
    Gen.choose(Int.MinValue, value - 1)

  def intsAboveValue(value: Int): Gen[Int] =
    Gen.choose(value + 1, Int.MaxValue)

  def intsOutsideRange(min: Int, max: Int): Gen[Int] =
    Gen.oneOf(intsBelowValue(min - 1), intsAboveValue(max + 1))

  def nonBooleans: Gen[String] =
    arbitrary[String]
      .suchThat(_.nonEmpty)
      .suchThat(_ != "true")
      .suchThat(_ != "false")

  def nonEmptyString: Gen[String] =
    arbitrary[String] suchThat (_.nonEmpty)

  def stringsWithMaxLength(maxLength: Int): Gen[String] =
    for {
      length <- choose(1, maxLength)
      chars  <- listOfN(length, alphaNumChar)
    } yield chars.mkString

  def extendedAsciiChar: Gen[Char] = chooseNum(128, 254).map(_.toChar)

  def extendedAsciiWithMaxLength(maxLength: Int): Gen[String] =
    for {
      length <- choose(1, maxLength)
      chars  <- listOfN(length, extendedAsciiChar)
    } yield chars.mkString

  def stringsLongerThan(minLength: Int, withOnlyPrintableAscii: Boolean = false): Gen[String] =
    for {
      length        <- Gen.chooseNum(minLength + 1, (minLength * 2).max(100))
      extendedAscii <- extendedAsciiChar
      chars <- {
        if (withOnlyPrintableAscii) {
          listOfN(length, Gen.alphaChar)
        } else {
          val listOfChar = listOfN(length, arbitrary[Char])
          listOfChar.map(_ ++ List(extendedAscii))
        }
      }
    } yield chars.mkString

  def stringsExceptSpecificValues(excluded: Seq[String]): Gen[String] =
    nonEmptyString suchThat (!excluded.contains(_))

  def oneOf[T](xs: Seq[Gen[T]]): Gen[T] =
    if (xs.isEmpty) {
      throw new IllegalArgumentException("oneOf called on empty collection")
    } else {
      val vector = xs.toVector
      choose(0, vector.size - 1).flatMap(vector(_))
    }

  def listWithMaxLength[A](maxLength: Int)(implicit a: Arbitrary[A]): Gen[List[A]] =
    for {
      length <- choose(1, maxLength)
      seq    <- listOfN(length, arbitrary[A])
    } yield seq

  def nonEmptyListOfMaxLength[A: Arbitrary](maxLength: Int): Gen[NonEmptyList[A]] =
    listWithMaxLength(maxLength).map(NonEmptyList.fromListUnsafe)

  def intWithMaxLength(maxLength: Int): Gen[Int] =
    for {
      length        <- choose(1, maxLength)
      listOfCharNum <- listOfN(length, numChar)
    } yield listOfCharNum.mkString.toInt

}
