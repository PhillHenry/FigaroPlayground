package com.phenry.figaro_playground.daphne_koller

import org.scalatest.{Matchers, WordSpec}

class LetterGradeSmartDifficultMainSpec extends WordSpec with Matchers {


  "probabilities" should {
    "flow X -> Y" in new ProbabilisticGraphicalModels {
      val chancesOfABefore = probabilityOf(defaultGradeDist, being(A))
      val chancesOfAAfter  = probabilityOf(gradeDistributionWhen(difficulty = easier), being(A))
      withClue(s"Before = $chancesOfABefore, After = $chancesOfAAfter") {
        // Before = 0.507, After = 0.5425
        chancesOfABefore should be < chancesOfAAfter
      }
    }

    "flow Y -> X" in new ProbabilisticGraphicalModels {
      val difficultyBefore  = probabilityOf(defaultDifficulty, being(Hard))
      whenGettingAnABecomesHarder()
      val difficultyAfter   = probabilityOf(defaultDifficulty, being(Hard))

      withClue(s"before = $difficultyBefore, after = $difficultyAfter") {
        // before = 0.6, after = 0.7378640776699029
        difficultyAfter should be > difficultyBefore
      }
    }

    "flow X -> W -> Y" in new ProbabilisticGraphicalModels {
      val letterChanceBefore  = probabilityOf(defaultLetterDist, being(Letter))
      whenTheCourseIsMoreLikelyToBeHard()
      val letterChanceAfter   = probabilityOf(defaultLetterDist, being(Letter))
      letterChanceBefore should be > letterChanceAfter
    }

    "flow Y -> W -> X ('evidential case')" in new ProbabilisticGraphicalModels {
      val difficultyBefore  = probabilityOf(defaultDifficulty, being(Hard))
      whenLetterBecomesLessLikely()
      val difficultyAfter   = probabilityOf(defaultDifficulty, being(Hard))
      difficultyBefore should be < difficultyAfter
    }

    "flow X <- W -> Y"  in new ProbabilisticGraphicalModels {
      val chancesOfABefore = probabilityOf(defaultGradeDist, being(A))
      defaultSatDist.observe(GoodSat) // note: it's not the intermediate variable that is changing
      val chancesOfAAfter = probabilityOf(defaultGradeDist, being(A))
      chancesOfAAfter should be > chancesOfABefore
    }

    "flow X -> W <- Y ('V-structure')" in new ProbabilisticGraphicalModels {
      // "If I tell you that a student took a class and the class is difficult, does that tell you anything
      // about the student's intelligence? And the answer is 'no'"

      // iff "W and all of its descendents are not observed"
      val smartBefore = probabilityOf(defaultIntelligence, being(Smart))
      whenTheCourseIsMoreLikelyToBeHard()
      val smartAfter  = probabilityOf(defaultIntelligence, being(Smart))
      smartBefore shouldEqual smartAfter
    }
  }

  "Given evidence about Z" should {

    "make no difference in (X -> W -> Y)" in new ProbabilisticGraphicalModels {
      // "I know the student got an A in the class, now I'm telling you that the class is really hard.
      // does that change the probability of the distribution of the letter?
      // No because ... the letter only depends on the grade"
      defaultGradeDist.observe(A)
      val letterChanceBefore  = probabilityOf(defaultLetterDist, being(Letter))
      whenTheCourseIsMoreLikelyToBeHard()
      val letterChanceAfter  = probabilityOf(defaultLetterDist, being(Letter))
      letterChanceBefore shouldEqual letterChanceAfter
    }

    "make no difference in X <- W -> Y" in new ProbabilisticGraphicalModels {
      // "If I tell you that the student is intelligent, then there is no way the SAT can influence
      // the probability influence in Grade"
      defaultIntelligence.observe(Smart)
      val chancesOfABefore = probabilityOf(defaultGradeDist, being(A))
      whenTheSatIsKnownToBeGood()
      val chancesOfAAfter = probabilityOf(defaultGradeDist, being(A))
      chancesOfAAfter shouldEqual chancesOfABefore
    }

    "X -> W <-Y (the V-Structure) " in new ProbabilisticGraphicalModels {
      // "Can difficulty influence intelligence via grade?"
      defaultGradeDist.observe(A)
      val chancesOfSmartBefore = probabilityOf(defaultIntelligence, being(Smart))
      whenTheCourseIsMoreLikelyToBeHard()
      val chancesOfSmartAfter = probabilityOf(defaultIntelligence, being(Smart))
      chancesOfSmartBefore should be < chancesOfSmartAfter
    }


    "flow X -> W <- Y ('V-structure')" in new ProbabilisticGraphicalModels {
      // Can difficulty influence intelligence via letter?
      val smartBefore = probabilityOf(defaultIntelligence, being(Smart))
      whenLetterBecomesLessLikely() // "this too activates the V-structure"
      val smartAfter  = probabilityOf(defaultIntelligence, being(Smart))
      smartBefore should be > smartAfter
    }
  }

}
