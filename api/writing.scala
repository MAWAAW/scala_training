package api

import java.io._

object Writing {

	def writeCivil(filename: String, content: List[String]): Unit = {
		val pw = new PrintWriter(new File(filename))
		pw.write(content.mkString("\n"))
		pw.close
	}

}