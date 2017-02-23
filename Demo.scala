import scala.util.{ Failure, Success, Try }

object Demo extends App {

	// Création d'une liste d'Int
	val l: List[Int] = List(1,-2,7,-6,4)

	// Affichage de tous éléments d'une liste d'Int.
	l.map(println)

	// Transformation d'une liste d'Int en une liste de leurs valeurs absolues.
	val l2 = l.map(v => if(v>0) v else -v)

	// Filtrage d'une liste d'Int en ne gardant que les éléments négatifs
	val l3 = l.filter(v => v<0)
	val l4 = filtrage(l)

	// Nombre d'éléments d'une liste d'Int
	println("nb element: "+nbElement(l))

	// Inversion d'une liste d'Int
	println("Inversion: "+inverse(l))
	println("Inversion: "+inverse2(l))

	// 5ème terme de la suite de Fibonacci
	println(fib(4))

	def filtrage(l: List[Int]): List[Int] = l match {
		case head :: tail => if (head<0) head::filtrage(tail) else filtrage(tail)
		case _ => Nil
	}

	def nbElement(l: List[Int]): Int = l match {
		case x::xs => 1 + nbElement(xs)
		case _ => 0
	}

	def inverse(l: List[Int]): List[Int] = {
		l.foldLeft(List[Int]())((a,b) => b::a)
	}

	def inverse2(l: List[Int]): List[Int] = l match {
		case x::xs => inverse2(xs) ::: List(x)
		case _ => Nil
	}

	def fib(n: Int): Int = {
		@annotation.tailrec
		def go(n: Int, p: Int, p2:Int): Int = {
			if (n <= 0) p2
			else go(n-1, p+p2, p)
		}
		go(n, 1, 0)
	}

	// Écrivez un programme permettant d'afficher les arguments passés en paramètre à la console
  	println(args.mkString("\n"))


}

