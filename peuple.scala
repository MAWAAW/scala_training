

object Peuple {	

	case class Personne(familyName: String, prenoms: List[String], metier: Option[String], age: Int, id: Int, ville: Ville, desirPolitique: Boolean, amis: Option[List[Personne]])
	case class Ville(name: String, latitude: Double, longitude: Double)
	case class Pays(name: String, villes: List[Ville])

	val LeMans = Ville("Le Mans", 34.5, 85.2)
	val Paris = Ville("Paris", 39.5, 107)
	val Munich = Ville("Munich", 99, 67.7)

	val France = Pays("France", List(LeMans,Paris))

	val Jalil = Personne("Daoumi", List("Jalil"), Some("developpeur"), 48, 1, LeMans, false, Some(List(Nicolas,Mahmat)))
	val Nicolas = Personne("Ducloux", List("Nicolas","Vincent"), Some("professeur"), 24, 2, Paris, false, Some(List(Mahmat)))
	val Mahmat = Personne("Annour", List("Mahmat"), None, 25, 3, LeMans, true, None)
	val Becken = Personne("Bauer", List("Becken"), None, 32, 4, Munich, true, None)


	def one(p: List[Personne], n: String): List[Personne] = {
		p match {
			case Personne(n,p,m,a,i,v,d,f) :: tail => Personne(n,p,m,a,i,v,d,f) :: one(tail, n)
			case _ :: tail => one(tail, n)
			case _ => Nil
		}
	}

	def two(p: List[Personne]): List[Personne] = {
		p match {
			case Personne(n,p,Some(m),a,i,v,d,f) :: tail => Personne(n,p,Some(m),a,i,v,d,f) :: two(tail)
			case Personne(_,_,None,_,_,_,_,_) :: tail => two(tail)
			case _ => Nil
		}
	}

	def three(p: List[Personne], v: Ville): Double = {
		def tmp(p: List[Personne], v: Ville): List[Double] = {
			p match {
				case Personne(_,_,_,a,_,v2,_,_) :: tail if(v2==v) => a :: tmp(tail,v)
				case _ :: tail => tmp(tail,v)
				case _ => Nil
			}
		}
		val l = tmp(p, v)
		l.sum / l.length
	}

	/*def four(p: List[Personne]): List[Ville] = {

	}*/

	def five(p: List[Personne], pays: Pays): List[Personne] = {
		p match {
			case Personne(n,p,m,a,i,v,d,f) :: tail if(pays.villes.contains(v)) => Personne(n,p,m,a,i,v,d,f) :: five(tail,pays)
			case _ :: tail => five(tail,pays)
			case _ => Nil
		}
	}

	def six(p: List[Personne], x: Int): (List[Personne],List[Personne]) = {
		def tmp(p: List[Personne], a: List[Personne], b: List[Personne], x: Int): (List[Personne],List[Personne]) = {
			p match {
				case Personne(n,p,m,age,i,v,d,f) :: tail => if(age<=x) tmp(tail,Personne(n,p,m,age,i,v,d,f)::a,b,x) else tmp(tail,a,Personne(n,p,m,age,i,v,d,f)::b,x)
				case _ => (a,b)
			}
		}
		tmp(p,List(),List(),x)
	}

	val l = List(Jalil,Nicolas,Mahmat,Becken)

	def main(args: Array[String]) {

		println(three(l,LeMans))

		five(l,France).map(println)

		println(six(l,27))

	}


}
