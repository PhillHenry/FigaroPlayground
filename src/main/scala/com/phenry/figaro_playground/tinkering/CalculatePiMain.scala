package com.phenry.figaro_playground.tinkering

import java.lang.Math.pow

import com.cra.figaro.algorithm.sampling.{MetropolisHastings, ProposalScheme}
import com.cra.figaro.language.{Apply, Chain, Constant}
import com.cra.figaro.library.atomic.continuous.Uniform

object CalculatePiMain {

  def length(x: Double, y: Double): Double = pow(pow(x, 2) + pow(y, 2), 0.5)

  def main(args: Array[String]): Unit = {
    val r = 100d
    val x = Uniform(-r, r)
    val y = Uniform(-r, r)

    val rSquared      = pow(r, 2)
    val applied       = Apply(x, y, (a: Double, b: Double) => length(a, b) < r)

    val radiusSqrd    = Chain(x, y, (a: Double, b: Double) => Constant(pow(a, 2) + pow(b, 2)))
    def predicate(r2: Double): Boolean = {
      println(r2)
      r2 < rSquared
    }


    // ratio of circle area to square:
    //  pi . r^2        pi
    // ------------  =  --
    // ( 2 . r )^2      4

//    val pAppliedTrue  = VariableElimination.probability(applied, true)
//    println("Estimate of pi = " + (pAppliedTrue * 4)) // 3.502222222222222, 2.7555555555555555, 3.591111111111111 ...
    val pMonteCarlo   = MetropolisHastings.probability(radiusSqrd, predicate _)
    println("Estimate of pi = " + (pMonteCarlo * 4)) // 3.144660000007077, 3.1374400000068694

    val mh            = MetropolisHastings(ProposalScheme(x, y), applied)
    mh.initialize()
    println(mh.computeDistribution(applied).take(10))
    println(mh.probability(radiusSqrd, predicate _))
  }

}
