package hw3

import hw3.Main.standardDeviation
import org.scalatest.{FunSuite, Matchers}

class StandardDeviationTest extends FunSuite with Matchers {

  test("stddev example") {
    /**
     * x	                 4      9    11   12    17     5     8    12    14
     * (x - avg(x))^2   38.7  1.49  0.60  3.16  45.9  27.3  4.94  3.16  14.3
     *
     * sum:    sum(x) = 139.55
     * avg:    139.55/9 = 15.51
     * stddev: sqrt(15.51) = 3.94
     **/
    standardDeviation(List(4, 9, 11, 12, 17, 5, 8, 12, 14)) shouldBe 3.93 +- 0.01
  }

  test("stddev singleton example") {
    /**
     * x	                  42
     * (x - avg(x))^2        0
     * stddev: sqrt(0) = 0
     **/
    standardDeviation(List(42)) shouldBe 0.0
  }

  test("empty list") {
    an [IllegalArgumentException] should be thrownBy standardDeviation(List.empty)
  }

  test("max value") {
    standardDeviation(List(Double.MaxValue)) shouldBe 0
  }

  test("two max values") {
    standardDeviation(List(Double.MaxValue, Double.MaxValue)) shouldBe 0
  }

  test("infinity") {
    an [Exception] should be thrownBy standardDeviation(List(Double.MaxValue, Double.MinValue))
  }

  test("large numbers, uniform distribution") {
    val vector = List(
      2.50429420e+153, -6.97571336e+153,  6.77846342e+153, 7.53295752e+153,
      3.84835680e+152, -5.95619642e+153, -6.68629368e+153, -2.13733745e+153,
      -3.84048834e+153, 6.96325298e+153,  5.14041390e+153, -9.63878832e+152,
      -7.81090867e+153, -4.95466607e+153, 4.92316267e+153, 6.41216591e+153,
      1.37810384e+153, -3.75066977e+153, -5.55306447e+153, -2.61032052e+153,
    )
    standardDeviation(vector) shouldBe 5.164255003006661381e+153 +- 1e+144
  }

  test("large numbers, exponential distribution") {
    val vector = List(
      1.40814105e+154, 1.83608855e+153, 1.19228173e+154, 1.57029250e+154,
      1.94314021e+153, 4.60671051e+153, 1.64085936e+153, 4.82204179e+153,
      1.30309818e+154, 1.30721051e+153, 3.71289868e+154, 2.75069522e+153,
      1.33766755e+153, 4.72355713e+153, 1.51174591e+154, 1.49093276e+154,
      1.41950190e+154, 1.56815798e+153, 7.37473785e+153, 7.21639285e+153,
    )
    standardDeviation(vector) shouldBe 8.4084689895108231004e+153 +- 1e+145
  }

  test("large numbers, normal distribution") {
    val vector = List(
      -2.49747051e+153, -5.89937014e+152,  5.56426301e+153,
      -4.80857083e+152,  1.06081575e+154, -1.77299205e+153,
      -4.09352395e+153, -4.23321914e+153, -1.53391727e+153,
      6.73568226e+153,  1.83749129e+154, -5.98027857e+153,
      -4.26710272e+153,  8.32780805e+152, -9.40379989e+153,
      -8.16501396e+152,  7.38117114e+153,  4.64909223e+153,
      -1.50488238e+153, -7.09568206e+153,
    )
    standardDeviation(vector) shouldBe 6.4849323943969715733e+153 +- 1e+145
  }

}
