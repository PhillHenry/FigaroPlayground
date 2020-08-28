package com.phenry.figaro_playground.bayes4hackers

import org.scalatest.{Matchers, WordSpec}

class BayesianMethodsForHackersSpec extends WordSpec with Matchers {

  import BayesianMethodsForHackers._

  "Elements" should {
    "be the same on different calls" in {
      val model = new Model
      import model._

      val someTaus = (1 to 100).map(_ => tau.value).toSet

      println(s"someTaus = $someTaus")

      withClue (someTaus.mkString(", ") + "\n") {
        someTaus.size shouldEqual 1
      }
    }
  }

  "Fitness functions" should {
    val real = 10
    val power = 2
    "be spot on for direct hit" in {
      fitnessFn(real)(real) shouldEqual 1.0
      fitnessFnPowered(real, power)(real) shouldEqual 1.0
    }
    "be smaller for a miss" in {
      fitnessFn(real)(11) shouldEqual 0.5
      fitnessFnPowered(real, power)(11) shouldEqual 0.25
    }
  }

}
