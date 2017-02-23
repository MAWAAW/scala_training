package api

import scala.util.{Try,Success,Failure}

object Reading {

	def read(filename: String): List[String] = {
		readTextFile(filename: String) match {
			case Success(lines) => lines
			case Failure(e) => List(e.toString)
		}
	}

	def readTextFile(filename: String): Try[List[String]] = {
		Try(scala.io.Source.fromFile(filename).getLines.toList)
	}

}