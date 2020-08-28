package com.phenry.scala.figaro

import org.scalatest.{Matchers, WordSpec}

class FigaroSpec extends WordSpec with Matchers {

  "Element having constraint added" should {

    "change probability" in new ProbabilisticGraphicalModels {
      val before  = probabilityOf(defaultDifficulty, being(Hard))
      whenTheCourseIsMoreLikelyToBeHard()
      val after   = probabilityOf(defaultDifficulty, being(Hard))
      withClue(s"before = $before, after = $after") {
        after should be > before
      }
    }

    "not be effected by probability not summing to 1.0" in new ProbabilisticGraphicalModels {
      val before  = probabilityOf(defaultDifficulty, being(Hard))
      whenTheCourseIsMoreLikelyToBeHard()
      val after   = probabilityOf(defaultDifficulty, being(Hard))
      println(s"before = $before, after = $after")
      after should be > before
    }

  }

}
