package com.phenry.scala.figaro

import com.cra.figaro.language.{Constant, Chain, Element}
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.library.atomic.discrete.Uniform

object MyFigaroBasics {

  def main(args: Array[String]): Unit = {
    val tau = Uniform((0 to 100).toList)
    println(tau.randomness)
    println((1 to 10).map(i => tau.value))

    val firms = (1 to 20).map(_.toLong)
    val winner: Element[Long] = discrete.Uniform(firms: _*)
    val fn: (Long) => Element[Long] = x => Constant(x)
    val winningBid: Element[Long] = Chain(winner, fn)
    println(winningBid)
    println(winner.generateValue(winner.generateRandomness())) // that's the puppy
  }

}
