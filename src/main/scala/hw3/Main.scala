package hw3

object Main {

  def standardDeviation(vector: List[Double]): Double = {
    require(vector.nonEmpty)
    val mean = vector.map(_ / vector.size).sum
    val variance = vector.map(v => (v - mean) / vector.size * (v - mean)).sum
    if (variance.isInfinity) {
      throw new Exception("I don't really wanna throw this one but I think my teacher wants me to...")
    }
    math.sqrt(variance)
  }

  def letterFrequencyRanking(corpus: String): String = {
    val preproc = corpus.filter(_.isLetter).toLowerCase
    val freq = preproc.foldLeft(Map[Char, Int]().withDefaultValue(0)) { (map, char) => map + (char -> (map(char) + 1)) }
    freq.toList.sortBy(_.swap)(Ordering.Tuple2(Ordering.Int.reverse, Ordering.Char)).map(_._1).mkString
  }

  def romanji(katakana: String): String = ???

  def gray(bits: Int): List[String] = {
    require(bits >= 0)
    if (bits == 0) List.empty
    else (2 to bits).foldLeft(List("0", "1")) { (prev, _) =>
      prev.map("0" + _) ++ prev.reverse.map("1" + _)
    }
  }

}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}
