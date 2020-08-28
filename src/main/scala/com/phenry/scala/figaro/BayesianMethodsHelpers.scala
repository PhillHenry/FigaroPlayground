package com.phenry.scala.figaro

import com.cra.figaro.algorithm.sampling.OneTimeProbQuerySampler
import com.cra.figaro.language.{Inject, Element}
import com.cra.figaro.library.atomic.discrete.AtomicUniform

import scala.io.Source._

/**
  * Created by henryp on 27/10/16.
  */
object BayesianMethodsHelpers {

  def main(args: Array[String]): Unit = {
    val data = getData()

    println(s"Number = ${data.length}, max = ${data.max}, min = ${data.min}")
  }

  def inject[T](xs: Seq[Element[T]]): Element[List[T]] = Inject(xs: _*)

  def getData(): List[Int] = {
    val bufferedSource  = fromFile(TestResources.testResourceFQN("BayesianMethodsForHackers/Chapter1_Introduction/data/txtdata.csv"))
    val lines           = bufferedSource.getLines().toList
    val data            = lines map { x =>
      val nMessages = java.lang.Double.valueOf(x).toInt
      //      println(nMessages)
      nMessages
    }
    data
  }

  def doComputation(mcmc: OneTimeProbQuerySampler, tau: AtomicUniform[Int]): List[Int] = {
    mcmc.start()
    //    mcmc.run()
    println("Model Distribution:")
    val tauDistro = mcmc.computeDistribution(tau)

    println(tauDistro.toSeq.sortBy(_._1).mkString(", "))
    val topTau = top5FirstGiveSeconds(tauDistro)
    println("Top 5 = " + topTau.mkString(", ")) // (3, 65, 58, 19, 15) (40, 47, 41, 30, 43)
    mcmc.kill()

    topTau.toList
  }

  def top5FirstGiveSeconds(tauDistro: Stream[(Double, Int)]): Seq[Int] = {
    tauDistro.toSeq.sortBy(_._1).reverse.take(5).map(_._2)
  }
}
