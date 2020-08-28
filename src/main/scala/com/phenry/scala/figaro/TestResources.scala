package com.phenry.scala.figaro

import java.io.File.separator

object TestResources {

  def testResourceFQN(filename: String, dir: String = "test"): String = {
    filenameFromRoot(s"src${separator}$dir${separator}resources${separator}$filename")
  }

  def filenameFromRoot(resource: String): String = {
    val tld = this.getClass.getResource(separator)
    s"${tld}..${separator}..${separator}$resource".substring(5)
  }

}
