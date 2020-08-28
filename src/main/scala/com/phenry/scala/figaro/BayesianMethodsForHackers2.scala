package com.phenry.scala.figaro

import java.lang.Math.{pow, abs}

import com.cra.figaro.algorithm.sampling.{ProposalScheme, MetropolisHastings}
import com.cra.figaro.language.{Inject, Universe, Element, Chain}
import com.cra.figaro.library.atomic.continuous.{Exponential, Beta}
import com.cra.figaro.library.atomic.discrete.{Uniform, AtomicUniform, Poisson}
import com.cra.figaro.library.compound.If
import com.phenry.scala.figaro.BayesianMethodsHelpers._

object BayesianMethodsForHackers2 {

  val data          = getData()
  val n_count_data  = data.length

  class Model {
    val mean  = data.sum.toDouble / data.length.toDouble
    val alpha = 1.0 / mean

    val lambda1: Element[Double]  = Exponential(alpha)
    val lambda2: Element[Double]  = Exponential(alpha)

    val poisson1: Element[Int]    = Poisson(lambda1)
    val poisson2: Element[Int]    = Poisson(lambda2)

    val tau: Element[Int]         = Uniform(0 to n_count_data: _*)

    def pDay(day: Int): Element[Int] = If (tau.map(_ > day), poisson1, poisson2)

    val modelData: Seq[Element[Int]] = (1 to n_count_data).map(d => pDay(d))
    modelData.zip(data).foreach { case(test, real) =>
      test.addConstraint(fitnessFnPowered(real, 4.75))
//      test.addConstraint(fitnessFn(real))
//      test.observe(real)
    }

    val model = Inject(modelData: _*)
  }

  def fitnessFn(real: Int): (Int) => Double = x => 1.0 / (abs(real - x) + 1)
  def fitnessFnPowered(real: Int, power: Double): (Int) => Double = x => 1.0 / pow((abs(real - x) + 1), power)


  /**
    * nSamples = 40k, burnin = 15k: (0.031825,45), (0.0322,46), (0.03515,19), (0.03865,43), (0.048625,44), (0.0522,38), (0.066625,15)
    * nSamples = 40k, burnin = 15k: (0.0359,45), (0.037766...,43), (0.041966..,48), (0.0456833..,49), (0.0473166..,22), (0.0529166..,44), (0.0794166..,28)
    * nSamples = 80k, burnin = 10k: (0.01595,45), (0.0171625,21), (0.0174875,73), (0.018075,6), (0.0184375,49), (0.0188,15), (0.0191,24), (0.0192375,39), (0.022025,43), (0.0221625,42), (0.025825,26), (0.0261625,32), (0.026575,30), (0.027775,31), (0.029125,48), (0.031425,47), (0.0314625,36), (0.035,38), (0.0377,27), (0.04065,29), (0.08075,44), (0.087425,28)
    * nSamples = 40k, burnin = 15k, addLogConstraint: (0.016925,44), (0.01695,3), (0.016975,15), (0.017525,57), (0.017525,2), (0.017775,62), (0.018725,30), (0.01915,4), (0.01955,61), (0.02015,73), (0.021375,20), (0.021775,6), (0.0225,0)
    *
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 2): (0.2405,28), (0.154675,15), (0.117725,29), (0.093625,44), (0.064775,36), (0.054225,26), (0.0443,48), (0.043825,33), (0.0404,37), (0.023125,49), (0.0169,38), (0.01355,22), (0.011625,43), (0.011275,30), (0.010825,61), (0.00995,31), (0.0095,41), (0.00865,39), (0.008575,34), (0.00645,64), (0.004725,23), (0.0039,17), (0.002475,27), (0.001625,20), (0.001275,16), (6.25E-4,0), (4.0E-4,14), (3.0E-4,21), (2.0E-4,45)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 3): (0.298025,22), (0.284,28), (0.112325,37), (0.068675,27), (0.056375,43), (0.056375,38), (0.031525,23), (0.026475,25), (0.02405,35), (0.012875,45), (0.00985,14), (0.006925,44), (0.005725,48), (0.005175,41), (0.001625,17)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4): (0.2446,28), (0.2354,15), (0.2098,44), (0.119975,29), (0.080425,27), (0.075925,22), (0.02275,39), (0.011125,43)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 5): (0.414,28), (0.30965,44), (0.0906,48), (0.060125,15), (0.049925,42), (0.039825,29), (0.020625,43), (0.0131,37), (0.00215,26)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 0.5): (0.049325,44), (0.036,74), (0.027675,73), (0.02705,6), (0.026125,0), (0.023475,22), (0.02175,8), (0.0216,4), (0.0209,43), (0.0208,32), (0.020225,28), (0.01995,5), (0.019325,31), (0.019125,50), (0.018925,29), (0.017475,1), (0.017275,72), (0.017175,13), (0.0171,61), (0.017075,46), (0.01675,20), (0.016375,16), (0.01605,39), (0.0155,33), (0.014375,63), (0.014375,41), (0.0143,45), (0.014175,2), (0.013625,40), (0.013325,71), (0.013225,68), (0.0132,18), (0.013025,15), (0.01275,10), (0.012425,17), (0.012375,69), (0.01225,14), (0.012075,59), (0.0117,66), (0.0117,60), (0.011575,3), (0.011175,37), (0.011025,12), (0.010925,62), (0.010125,47), (0.0099,70), (0.009625,11), (0.0095,25), (0.0092,51), (0.009125,23), (0.008975,34), (0.00895,64), (0.0089,7), (0.0086,49), (0.008575,35), (0.00855,67), (0.008325,26), (0.008175,48), (0.007925,58), (0.00765,19), (0.00725,38), (0.00705,9), (0.006825,55), (0.006575,24), (0.00655,54), (0.006025,42), (0.00565,27), (0.005575,53), (0.00555,57), (0.0048,65), (0.0046,52), (0.003975,30), (0.003975,56), (0.00355,21), (0.003325,36)
    * nSamples = 40k, burnin = 10k, fitnessFn: (0.083675,42), (0.074375,44), (0.046975,38), (0.0401,26), (0.033525,49), (0.031175,40), (0.0297,15), (0.029225,39), (0.029175,48), (0.027475,45), (0.027475,28), (0.0273,41), (0.025425,34), (0.024025,32), (0.022125,37), (0.01935,22), (0.017275,21), (0.016575,20), (0.0165,30), (0.0165,29), (0.016,50), (0.015675,47), (0.0144,14), (0.014225,43), (0.0142,13), (0.01345,24), (0.013425,74), (0.012575,25), (0.01225,70), (0.0121,23), (0.011475,36), (0.011375,46), (0.010825,54), (0.009325,73), (0.009025,33), (0.008875,60), (0.00885,31), (0.0086,4), (0.008425,5), (0.008325,61), (0.00825,35), (0.0079,0), (0.0078,72), (0.007575,8), (0.007275,69), (0.007275,52), (0.006975,51), (0.006475,18), (0.006075,6), (0.005325,16), (0.00525,3), (0.00495,7), (0.00435,1), (0.0041,11), (0.0039,27), (0.003675,66), (0.00325,19), (0.002575,63), (0.002475,65), (0.00245,58), (0.0023,68), (0.002225,64), (0.00195,53), (0.001825,2), (0.0018,62), (0.00165,59), (0.001625,9), (0.0015,12), (0.001325,57), (0.001025,56), (0.001025,71), (9.25E-4,10), (8.0E-4,17), (7.75E-4,67)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 5), value > day: (1.0,45)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 3), value > day: (0.348525,29), (0.318175,16), (0.13905,39), (0.067775,44), (0.04435,28), (0.02745,45), (0.023375,38), (0.0215,25), (0.006625,50), (0.002925,19), (2.5E-4,48)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4), value > day: (0.29545,16), (0.276075,29), (0.15995,45), (0.088375,28), (0.079125,39), (0.070125,43), (0.0309,27)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4.5), value > day: (0.78225,45), (0.08925,38), (0.0818,32), (0.03885,39), (0.00785,29)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4.75), value > day: (0.76965,28), (0.1727,29), (0.05765,45)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4.75), value > day: (0.4303,45), (0.261975,39), (0.25425,29), (0.0458,44), (0.0061,23), (0.001575,38)
    * nSamples = 40k, burnin = 10k, fitnessFnPowered(power = 4.875), value > day: (0.87425,45), (0.0899,43), (0.0249,38), (0.01095,39)
    */
  def main(args: Array[String]): Unit = {
    val model = new Model
    val nSamples = 40000
    val mh = MetropolisHastings(numSamples = nSamples,
      ProposalScheme(model.tau, model.lambda1, model.lambda2),
      burnIn = 10000,
      model.tau)
//    doComputation(mh, model.tau)
    mh.start()
    mh.run()
    println("Model Distribution:")
    val tauDistro = mh.computeDistribution(model.tau)

    println(tauDistro.toSeq.sortBy(_._1).reverse.mkString(", "))
  }


}
