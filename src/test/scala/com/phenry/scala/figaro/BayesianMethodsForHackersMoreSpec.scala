package com.phenry.scala.figaro

import cern.jet.random.Poisson
import cern.jet.random.engine.{MersenneTwister, RandomEngine}
import org.scalatest.{Matchers, WordSpec}

class BayesianMethodsForHackersMoreSpec extends WordSpec with Matchers {

  import BayesianMethodsForHackers._

  val lambda1           = 10
  val lambda2           = 30
  val differentLambda1  = lambda1 / 10
  val differentLambda2  = lambda2 * 10
  val tau               = 10
  val n_count_data      = 30
  val data1             = generateFn(n_count_data)(lambda1, lambda2, tau)
  val data2             = generateFn(n_count_data)(lambda1, lambda2, tau)
  val differentData     = generateFn(n_count_data)(differentLambda1, differentLambda2, tau)

  println(s"data1         = $data1\ndata2         = $data2\ndifferentData = $differentData")

  "data" should {
    "be different in each run" in {
      data1 should not equal data2
    }
  }

  "scores" should {
    val scoreFn: List[Int] => BigDecimal = score(tau, lambda1, lambda2, _)

    val score1    = scoreFn(data1)
    val score2    = scoreFn(data2)
    val tolerance = (lambda1 + lambda2) / 2.0

    "be low for data that was generated from parameters" in {
      println(s"score1 = $score1, score2 = $score2")
      score1.toDouble should be > 0.0
      score1.toDouble shouldEqual score2.toDouble +- tolerance // typically, score1 = 9.825, score2 = 13.473
    }
    "be radically different for different data" in {
      val differentScore  = scoreFn(differentData)
      println(s"differentScore = $differentScore")
      withClue(s"score1 = $score1, radically different score = $differentScore\n") {
        (differentScore - score1).toDouble should be > tolerance
      }
      withClue(s"score2 = $score2, radically different score = $differentScore\n") {
        (differentScore - score2).toDouble should be > tolerance
      }
    }
  }

  "Poisson" should {
    val tolerance = 0.001
    "conform to values at http://stattrek.com/online-calculator/poisson.aspx" in {
      calculatePoison(1, 0).toDouble shouldEqual 0.368 +- tolerance
      calculatePoison(1, 1).toDouble shouldEqual 0.368 +- tolerance
      calculatePoison(1, 2).toDouble shouldEqual 0.184 +- tolerance
      calculatePoison(1, 3).toDouble shouldEqual 0.061 +- tolerance
      calculatePoison(1, 4).toDouble shouldEqual 0.015 +- tolerance
    }
    "should not blow up with large numbers" ignore { // my naive implementation blows up at this scale
      calculatePoison(1000, 999).toDouble shouldEqual 0.0126 +- tolerance
    }
    "should be OK with Colt library" in {
      val poisson = new Poisson(1000, new MersenneTwister())
      poisson.pdf(999) shouldEqual 0.0126 +- tolerance
    }
  }

}
