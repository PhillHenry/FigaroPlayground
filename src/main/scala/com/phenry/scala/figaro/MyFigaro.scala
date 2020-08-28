package com.phenry.scala.figaro

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Exponential
import com.cra.figaro.library.atomic.discrete.Poisson
import com.cra.figaro.library.compound.{CPD2, CPD, CPD1}

object MyFigaro {

  def main(args: Array[String]): Unit = {
    val exponential = Exponential(10)
//    println(s"exponential = ${exponential.generateValue()}")
    //val observation     = Poisson(Constant(10d))
    val observation     = Poisson(exponential)
    (1 to 10).foreach { i =>
      println(s"#$i: ${observation.generateValue()}")
    }

    bookExamples()

    observingGradeChangesOutcome()

    flowOfProbabilities()

    val model = new ProbabilisticGraphicalModels
    import model._

    gradeDistributionWhen().observe(B)
    gradeDistributionWhen().setCondition((x: String) => x == B)
    val ve = VariableElimination(defaultIntelligence, defaultDifficulty)
    ve.start()
    println(ve.probability(defaultDifficulty, (x: String) => x == Hard))

    println()
  }

  def flowOfProbabilities(): Unit = {
    val model = new ProbabilisticGraphicalModels
    import model._

    val ve = VariableElimination(defaultDifficulty)
    ve.start()
    println("Difficult? " + ve.probability(defaultDifficulty, (x: String) => x == Hard))


    //    model.satDist.observe(BadSat)
    //    chancesOf(Letter, letterDist) // 0.7121474782608695

    gradeDistributionWhen().observe(C)
    val ve2 = VariableElimination(defaultDifficulty)
    ve2.start()
    println("Difficult? " + ve2.probability(defaultDifficulty, (x: String) => x == Hard))

    satDist().observe(BadSat)
    chancesOf(Letter, letterDist()) // 0.010000000000000002

    val ve3 = VariableElimination(defaultDifficulty)
    ve3.start()
    println("Difficult? " + ve3.probability(defaultDifficulty, (x: String) => x == Hard))
  }

  def observingGradeChangesOutcome(): Unit = {
    val model = new ProbabilisticGraphicalModels
    import model._
    chancesOf(Letter, letterDist()) // 0.6036560000000001
    gradeDistributionWhen().observe(A)
    chancesOf(Letter, letterDist()) // 0.9
  }

  def chancesOf(outcome: String, distribution: CPD1[String, String]): Double = {
    val ve          = VariableElimination(distribution)
    ve.start()
    val probability = ve.probability(distribution, outcome)
    println(s"Chances of $outcome is $probability")
    probability
  }

  def bookExamples(): Unit = {
    val burglary = Flip(0.01)
    val earthquake = Flip(0.0001)

    val economicClimateGood = Flip(0.5)
    println(economicClimateGood.prob)
  }
}

class ProbabilisticGraphicalModels {

  implicit val universe = Universe.createNew()

  val Easy          = "Easy"
  val Hard          = "Hard"
  val Dumb          = "Dumb"
  val Smart         = "Smart"
  val A             = "A"
  val B             = "B"
  val C             = "C"
  val GoodSat       = "GoodSat"
  val BadSat        = "BadSat"
  val Letter        = "Letter"
  val NoLetter      = "NoLetter"

  def chancesOfDifficultIs(d: Double): Chain[Boolean, String]
    = Chain(Flip(d), (b: Boolean) => if (b) Constant(Hard) else Constant(Easy))

  def chancesOfSmartIs(d: Double): Chain[Boolean, String]
    = Chain(Flip(d), (b: Boolean) => if (b) Constant(Smart) else Constant(Dumb))

  def gradeDistributionWhen(intelligence: Chain[Boolean, String] = defaultIntelligence,
                            difficulty:   Chain[Boolean, String] = defaultDifficulty): CPD2[String, String, String]
    = CPD(intelligence, difficulty,
    (Dumb, Easy)   -> Select(0.3   -> A,   0.4   -> B,   0.3   -> C),
    (Dumb, Hard)   -> Select(0.05  -> A,   0.25  -> B,   0.7   -> C),
    (Smart, Easy)  -> Select(0.9   -> A,   0.08  -> B,   0.02  -> C),
    (Smart, Hard)  -> Select(0.5   -> A,   0.3   -> B,   0.2   -> C)
  )

  def satDist(intelligence: Chain[Boolean, String] = defaultIntelligence): CPD1[String, String]
    = CPD(intelligence,
    Dumb        -> Select(0.95  -> BadSat,  0.05  -> GoodSat),
    Smart -> Select(0.2   -> BadSat,  0.8   -> GoodSat)
  )

  def letterDist(gradeDist: CPD2[String, String, String] = defaultGradeDist): CPD1[String, String]
    = CPD(gradeDist,
    A -> Select(0.1   -> NoLetter,  0.9   -> Letter),
    B -> Select(0.4   -> NoLetter,  0.6   -> Letter),
    C -> Select(0.99  -> NoLetter,  0.01  -> Letter)
  )

  val defaultDifficulty   = chancesOfDifficultIs(0.6)
  val easier              = chancesOfDifficultIs(0.5)
  val defaultIntelligence = chancesOfSmartIs(0.7)
  val defaultGradeDist    = gradeDistributionWhen(intelligence = defaultIntelligence, difficulty = defaultDifficulty)
  val defaultLetterDist   = letterDist(defaultGradeDist)
  val defaultSatDist      = satDist(defaultIntelligence)

  def being(x: String): (String) => Boolean = _ == x

  def probabilityOf[T](target: Element[T], fn: (T) => Boolean): Double = {
    val ve = VariableElimination(target)
    ve.start()
    ve.probability(target, fn)
  }

  def whenGettingAnABecomesHarder(): Unit = defaultGradeDist.addConstraint(x => if (x == A) 0.1 else 0.9)

  def whenTheCourseIsMoreLikelyToBeHard(): Unit = defaultDifficulty.addConstraint(x => if (x == Hard) 0.99 else 0.01)

  def whenLetterBecomesLessLikely(): Unit = defaultLetterDist.addConstraint(x => if (x == Letter) 0.1 else 0.9)

  def whenTheSatIsKnownToBeGood(): Unit = defaultSatDist.observe(GoodSat)
}
