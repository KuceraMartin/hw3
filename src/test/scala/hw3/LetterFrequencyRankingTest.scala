package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {

  test("Simple") {
    letterFrequencyRanking("hello") shouldBe "leho"
  }

  test("Capital letters") {
    letterFrequencyRanking("AaaAaaAaa") shouldBe "a"
  }

  test("Punctuation") {
    letterFrequencyRanking("Sic!") shouldBe "cis"
  }

  test("from readme") {
    letterFrequencyRanking("Sic semper tyranis.") shouldBe "seiracmnpty"
  }

  test("empty string") {
    letterFrequencyRanking("") shouldBe ""
  }

  test("almost empty string") {
    letterFrequencyRanking("!@#$%") shouldBe ""
  }

}
