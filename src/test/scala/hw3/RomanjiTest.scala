package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {

  test("Toilet")     { romanji("トイレ") shouldBe "toire"  }
  test("Television") { romanji("テレビ") shouldBe "terebi" }
  test("Drama")      { romanji("ドラマ") shouldBe "dorama" }
  test("Ice-cream")  { romanji("アイスクリーム") shouldBe "aisukurīmu" }
  test("Knock")      { romanji("ノック") shouldBe "nokku"}

  test("empty string")      { romanji("") shouldBe ""}

  test("n")      { romanji("ン") shouldBe "n"}

  test("ki")      { romanji("キ") shouldBe "ki"}
  test("kya")      { romanji("キャ") shouldBe "kya"}
  test("kyu")      { romanji("キュ") shouldBe "kyu"}
  test("kyo")      { romanji("キョ") shouldBe "kyo"}

  test("saka")      { romanji("サカ") shouldBe "saka"}
  test("sakka")      { romanji("サッカ") shouldBe "sakka"}

  test("nino")      { romanji("ニノ") shouldBe "nino"}
  test("ninno")      { romanji("ニンノ") shouldBe "ninno"}

  test("meru")      { romanji("メル") shouldBe "meru"}
  test("mēru")      { romanji("メール") shouldBe "mēru"}

  test("kkyā")      { romanji("ッキャー") shouldBe "kkyā"}

  test("non-convertible")      { romanji("blah blah ガャ ンー") shouldBe "blah blah gaャ nー"}

}
