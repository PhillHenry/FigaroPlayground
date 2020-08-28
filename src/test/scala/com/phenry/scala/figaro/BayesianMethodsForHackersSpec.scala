package com.phenry.scala.figaro

import com.cra.figaro.algorithm.sampling.{MetropolisHastings, ProposalScheme}
import com.cra.figaro.language.{Constant, Element}
import com.cra.figaro.library.atomic.continuous.Exponential
import com.cra.figaro.library.atomic.discrete.Poisson
import com.phenry.scala.figaro.BayesianMethodsHelpers.top5FirstGiveSeconds
import org.scalatest.{Matchers, WordSpec}

class BayesianMethodsForHackersSpec extends WordSpec with Matchers {

  import BayesianMethodsForHackers._

  trait BayesianMethodsForHackersFixture {
    val actualLambda1   =  0.01
    val actualLambda2   =  0.1
    val randomizeBefore = Poisson(Exponential(actualLambda1))
    val randomizeAfter  = Poisson(Exponential(actualLambda2))
    val actualTau   = 45
    val actualCount = 100
    val actualData  = (Array.fill(actualTau)(randomValueFrom(randomizeBefore)) ++ Array.fill(actualCount - actualTau)(randomValueFrom(randomizeAfter))).toList
    val different   = actualData.reverse
    val world       = new Variables(actualData)
  }

  "logp" should {
    "represent log 10" in {
      logp(BigDecimal("0.001")) shouldEqual -3
      logp(BigDecimal("0.01")) shouldEqual -2
      logp(BigDecimal("0.1")) shouldEqual -1
      logp(BigDecimal("0.3")).toDouble shouldEqual (0 - 0.522878745) +- 0.001
      logp(BigDecimal("1.0")) shouldEqual 0
      logp(BigDecimal("10")) shouldEqual 1
    }
  }

  "factorial" should {
    "be expected" in {
      factorial(0) shouldEqual 1
      factorial(1) shouldEqual 1
      factorial(2) shouldEqual 2
      factorial(3) shouldEqual 6
      factorial(4) shouldEqual 24
    }
  }

  "poisson" should {
    "follow the distribution found at StatTrek.com's calculator" in {
      poisson(lambda = 4)(5).toDouble shouldEqual 0.156 +- 0.01
      poisson(lambda = 4)(4).toDouble shouldEqual 0.195 +- 0.01
      poisson(lambda = 10)(10).toDouble shouldEqual 0.125 +- 0.01
    }
    "not roound to zero" in {
      poisson(lambda = 0.01)(311) should not equal 0d
    }
  }

  "MH" should {
    "yield tau" ignore new BayesianMethodsForHackersFixture {
      import world._
      val proposalScheme  = ProposalScheme(lambda_1, lambda_2, tau)
      val mh              = MetropolisHastings(numSamples = 12000,
        proposalScheme,
        burnIn = 2000,
        tau)
      compute(data, world, mh, tau).head shouldEqual actualTau +- 3
    }
  }

  "infinities" should {
    "be summable" in {
      val probabilities = Seq(0d, 1d, 2d).map(BigDecimal(_))
//      withClue(probabilities.map(logp).mkString(", ")) {
//        _sum_deviance(probabilities) shouldBe Double.NegativeInfinity
//      }
    }
  }

  "deviance" should {
    "always be a number" in {
//      _sum_deviance(Array.fill[Double](2)(Double.MinValue)) should be > Double.MinValue // no! it's < 0
    }
    "log should not be a NaN" in {
//      logp(Double.MinValue) should not equal Double.NaN yes it should as MinValue < 0
    }
  }

  "Actual values" should {
    "not be unlikely" in new BayesianMethodsForHackersFixture {
      for (i <- 1 to 100) {
        withClue(s"Failed on iteration $i") {
          score(actualTau, actualLambda1, actualLambda2, actualData).toDouble should be > Double.MinValue
        }
      }
    }
  }

  "scores" should {
    "change when we deviate given params" ignore new BayesianMethodsForHackersFixture {
      import world._
      val expectedBest = score(actualTau, actualLambda1, actualLambda2, actualData)

      def stepSize(d: Double, nSteps: Int): (Double, Double) = {
        val max = d * 2
        val stepSize = max / nSteps
        (stepSize, max)
      }

      val (l1StepSize, maxL1) = stepSize(actualLambda1, 5)
      val (l2StepSize, maxL2) = stepSize(actualLambda2, 5)
      val tolerance           = 0.3
      for (t <- 0 to actualData.length) {
        var l1 = actualLambda1 / 10
        while (l1 < maxL1) {
          if (l1 != actualLambda1) {
            var l2 = actualLambda2 / 10
            while (l2 < maxL2) {
              if (l2 != actualLambda2) {
                println(s"t = $t, l1 = $l1, l2 = $l2")
                val currentScore = score(t, l1, l2, data)
                println(s"currentScore = $currentScore")
                withClue(s"l1 = $l1, actual lambda 1 = $actualLambda1, l2 = $l2, actual lamdba 2 = $actualLambda2, t = $t: ") {
                  currentScore should be < (expectedBest + tolerance)
                  l2 += l2StepSize
                }
              }
            }
          }
          l1 += l1StepSize
        }
      }
    }

    "change when we deviate" ignore new BayesianMethodsForHackersFixture {
      import world._

      applied.set(actualData)
      applied.setConstraint(fitnessFn(world))
      applied.score(actualData, actualData) shouldEqual 0d
      applied.score(different, actualData) should be > 0d
      applied.score(actualData, different) should be < 0d
    }
  }

  "splitting days" should {
    val lambda_1 = 1d
    val lambda_2 = 2d
    val fn: Double => Element[Double] = { x =>
      val const = Constant(x)
      const.observe(x)
      const
    }


    "have the same number of days" in {
      for (n_count_data <- 1 to 10) {
        for (tau <- 1 to n_count_data) {
          val elements = lambda_(n_count_data, tau, lambda_1, lambda_2, fn)
          withClue(s"n_count_data = $n_count_data, tau = $tau, elements = " + elements.mkString(", ")) {
            elements should have size n_count_data
            elements.take(tau).foreach { element =>
              element.value shouldEqual lambda_1
            }
          }
        }
      }
    }
  }

  "sorting" should {
    "yield top X" in {

      val numbers = (1 to 10).map(x => (x.toDouble, x))
      val top5 = top5FirstGiveSeconds(numbers.toStream)
      top5 should have size 5

      top5.head shouldEqual 10
    }
  }

}
