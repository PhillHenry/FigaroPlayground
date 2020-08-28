package com.phenry.scala.figaro

import java.lang.Math.{max, abs, exp, min}

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Exponential
import com.cra.figaro.library.atomic.discrete.{AtomicUniform, Poisson, Uniform}
import com.phenry.scala.figaro.BayesianMethodsHelpers._

import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.util.{Failure, Success, Try}

object BayesianMethodsForHackers {

  def toConstant(x: Double): Element[Double] = Constant(x)
  def toConstant(x: Int): Element[Int] = Constant(x)

  def toPoisson(x: Double): Element[Int] = Poisson(x)

  def main(args: Array[String]): Unit = {
    try {
      val tried = Try(doRun())
      tried match {
        case Success(_) => println("Run without error")
        case Failure(x) => x.printStackTrace()
      }
    } catch {
      case t: Throwable => t.printStackTrace()
    } finally {
      System.exit(-1)
    }
  }

  class Variables(val data: List[Int]) {
    val n_count_data                        = data.length
    val tau: AtomicUniform[Int]             = Uniform(0 to n_count_data: _*)("tau", Universe.universe)
    val mean                                = data.sum.toDouble / n_count_data.toDouble
    val alpha                               = 1 / mean
    val lambda_1: Element[Double]           = Exponential(alpha)("lambda_1", Universe.universe)
    val lambda_2: Element[Double]           = Exponential(alpha)("lambda_2", Universe.universe)

    println(s"alpha = $alpha, n_count_data = $n_count_data, mean = $mean, sum = ${data.sum}, data = [ ${data.take(5).mkString(", ")} ... ${data.drop(n_count_data - 5).mkString(", ")}]")
    println(s"lambda_1 = $lambda_1")

    val applied: Apply3[Double, Double, Int, List[Int]] = Apply(lambda_1, lambda_2, tau, generateNothing) //generateFn(data.length))
  }

  val generateNothing: (Double, Double, Int) => List[Int] = (l1, l2, t) => List()

  def generateFn(length: Int): (Double, Double, Int) => List[Int] = (l1, l2, t) => {
    val xs: List[Element[Int]] = lambda_(length, t, l1, l2, toPoisson)
    xs.map{x =>
      val value = randomValueFrom(x)
      value
    }
  }

  def randomValueFrom(x: Element[Int]): x.Value = {
    x.generateValue(x.generateRandomness())
  }

  def doRun(): Unit = {
    val data = getData()
    val world = new Variables(data)
    import world._

    val proposalScheme                                  = ProposalScheme(lambda_1, lambda_2, tau)
    val mh                                              = MetropolisHastings(numSamples = 40000,
                                                                            proposalScheme,
                                                                            burnIn = 10000,
                                                                            tau)
    compute(data, world, mh, tau)
  }


  def lambda_[T](n_count_data: Int, tau: Int, lambda_1: Double, lambda_2: Double, fn: Double => Element[T]): List[Element[T]] = {
    val before              = if (tau > 0) Array.fill[Element[T]](tau)(fn(lambda_1)) else Array[Element[T]]()
    val afterAndIncluding   = if (tau < n_count_data) Array.fill[Element[T]](n_count_data - tau)(fn(lambda_2)) else Array[Element[T]]()
    val out                 = before ++ afterAndIncluding
    out.toList
  }

  def compute(data: List[Int],
              variables: Variables,
              mcmc: OneTimeProbQuerySampler,
              tau: AtomicUniform[Int]): List[Int] = {
    val model = variables.applied
    model.addConstraint(fitnessFn(variables))
    model.set(data)
//    mcmc.initialize()
    doComputation(mcmc, tau)
  }

  val factorialQuickly = scala.collection.mutable.Map[Int, BigDecimal]()
  def factorial(k: Int): BigDecimal = {
    @tailrec
    def doFactorial(acc: BigDecimal, n: Int): BigDecimal = {
      if (n == 0) {
        acc
      } else {
        doFactorial(acc * n, n-1)
      }
    }

    factorialQuickly.getOrElse(k, {
      val factorialed = doFactorial(1, k)
      factorialQuickly(k) = factorialed
      factorialed
    })
  }

  val poissonQuickly = scala.collection.mutable.Map[(Double, Int), BigDecimal]()
  def poisson(lambda: Double)(k: Int): BigDecimal = {

    poissonQuickly.getOrElse((lambda, k), {
      val pBigDecimal: BigDecimal = calculatePoison(lambda, k)
      poissonQuickly((lambda, k)) = pBigDecimal

      pBigDecimal
    })
  }

  def calculatePoison(lambda: Double, k: Int): BigDecimal = {
    val bigLambda   = BigDecimal(lambda)
    val factorialed = factorial(k)
    val pBigDecimal = (bigLambda.pow(k) / factorialed) * exp(-lambda)
    pBigDecimal
  }

  /**
    * @return ideally between 0 and 1 ("works best"). 1 is ideal, 0 is non matching.
    * @see https://forums.manning.com/posts/list/37881.page
    */
  def fitnessFn(variables: Variables): List[Int] => Double = { estimated =>
//    println(estimated.mkString(", "))
//    val model = variables.applied
//    println("values = " + model.args.map(_.value).mkString(", ")) // these do indeed change every iteration...
    import variables._

    val tauV      = tau.value
    val lambda1V  = lambda_1.value
    val lambda2V  = lambda_2.value
    val sum       = score(tauV, lambda1V, lambda2V, data)
//    println(s"tauV = $tauV, lambda1V = $lambda1V, lambda2V = $lambda2V, sum = $sum")
//    readLine()
    val x         = max(1, abs(sum.toDouble))
    1/x
//    x
  }

  def score(tauV: Int, lambda1V: Double, lambda2V: Double, data: List[Int]): BigDecimal = {
    val poisson1 = poisson(lambda1V) _
    val poisson2 = poisson(lambda2V) _
    val probabilities = data.take(tauV).map(poisson1) ++ data.drop(tauV).map(poisson2)

    _sum_deviance(probabilities)
  }

  // see _sum_deviance in Model.py in PYMC
  def _sum_deviance(probabilities: Seq[BigDecimal]): BigDecimal = {
    // return -2 * sum([v.get_logp() for v in self.observed_stochastics])
     -2 *
       probabilities.map(logp).sum
  }

  /**
    * see http://stackoverflow.com/questions/11848887/bigdecimal-to-the-power-of-bigdecimal-on-java-android/22556217#22556217
    */
  def logp(x: BigDecimal): BigDecimal = {
    val decimalPlacesTo1stDigit = x.precision - x.scale - 1
    decimalPlacesTo1stDigit + Math.log10(x.underlying().movePointLeft(decimalPlacesTo1stDigit).doubleValue())
  }

  def firstDigit(x: BigDecimal): Int = {
    val scale = x.scale
    val precision = x.precision
    x.underlying().movePointLeft(precision-scale-1).abs().intValue()
  }
}
