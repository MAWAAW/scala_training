import scala.util.Random

object Pokemon {	

	case class Dresseur(name: String, age: Int)
	case class Pokemon(name: String, ptype: Option[String], evolution: Option[String], dresseur: Option[Dresseur], level: Int)
	// ptype: feu, eau, plante, electrique, glace

	val Sacha = Dresseur("Sacha",11)
	val Pierre = Dresseur("Pierre",15)
	val Ondine = Dresseur("Ondine",13)

	val Pikachu = Pokemon("pikachu",Some("electrique"),Some("raichu"),Some(Sacha),9)
	val Bulbizar = Pokemon("bulbizar",Some("plante"),Some("herbizar"),Some(Sacha),7)
	val Onyx = Pokemon("onyx",Some("electrique"),None,Some(Pierre),14)
	val Smogo = Pokemon("smogo",Some("eau"),Some("smogogo"),Some(Pierre),7)
	val Bulbizar2 = Pokemon("bulbizar",Some("plante"),Some("herbizar"),Some(Pierre),6)
	val Ronflex = Pokemon("ronflex",None,None,Some(Ondine),16)
	val Mweo = Pokemon("mweo",None,None,None,99)

	val p: List[Pokemon] = List(Pikachu,Bulbizar,Onyx,Smogo,Bulbizar2,Ronflex,Mweo)
	val d: List[Dresseur] = List(Sacha,Pierre,Ondine)

	// La liste des pokemon d'un niveau supérieur à "x"
	def pokHigherThan(p:List[Pokemon], x:Int): List[Pokemon] = {
		p match {
			case Pokemon(n,t,e,d,l)::tail => if(l>x) Pokemon(n,t,e,d,l)::pokHigherThan(tail,x) else pokHigherThan(tail,x)
			case _ => Nil
		}
	}

	// La liste des pokemon ayant un dresseur
	def pokWithDresseur(p:List[Pokemon]): List[Pokemon] = {
		p match {
			case Pokemon(n,t,e,Some(d),l)::tail => Pokemon(n,t,e,Some(d),l)::pokWithDresseur(tail)
			case Pokemon(n,t,e,None,l)::tail => pokWithDresseur(tail)
			case _ => Nil
		}
	}

	// La liste de toutes les évolutions (sans doublon)
	def allEvolution(p:List[Pokemon]): List[String] = {
		p match {
			case Pokemon(_,_,Some(e),_,_)::tail => e::allEvolution(tail).distinct
			case Pokemon(_,_,None,_,_)::tail => allEvolution(tail)
			case _ => Nil
		}
	}

	// La liste des pokemon ayant un dresseur dont l'âge est supérieur à "a"
	def pokByDresseurAge(p:List[Pokemon],a:Int): List[Pokemon] = {
		p match {
			case Pokemon(n,pt,e,Some(d),l)::tail if(d.age>a) => Pokemon(n,pt,e,Some(d),l)::pokByDresseurAge(tail,a)
			case Pokemon(_,_,_,_,_)::tail => pokByDresseurAge(tail,a)			
			case _ => Nil
		}
	}

	// La liste des pokemon ayant un dresseur dont le nom commence par "s"
	def pokByDresseurName(p:List[Pokemon],s:Char): List[Pokemon] = {
		p match {
			case Pokemon(n,pt,e,Some(d),l)::tail if(d.name.charAt(0)==s) => Pokemon(n,pt,e,Some(d),l)::pokByDresseurName(tail,s)
			case Pokemon(_,_,_,_,_)::tail => pokByDresseurName(tail,s)			
			case _ => Nil
		}
	}

	// La moyenne d'âge des dresseurs
	def meanDresseurAge(p:List[Dresseur]): Double = {
		def mean(p:List[Dresseur],nb:Int): List[Double] = {
			p match {
				case Dresseur(_,age)::tail => age::mean(tail,nb+1)
				case _ => Nil
			}
		}
		val l = mean(p,0)
		l.sum / l.length
	}

	// Un tuple (Int,Int) comprenant respectivement le niveau min et max de la liste des pokemons
	def pokMinMaxLvl(p:List[Pokemon]): (Int,Int) = {
		def minmax(p:List[Pokemon]): List[Int] = {
			p match {
				case Pokemon(_,_,_,_,l)::tail => l::minmax(tail)
				case _ => Nil
			}
		}
		val l = minmax(p)
		(l.min,l.max)
	}

	// La liste des dresseurs ayant des pokemons de type "t"
	def dresseurByPokOfThisType(p:List[Pokemon], t:String): List[Dresseur] = {
		p match {
			case Pokemon(n,Some(pt),e,Some(d),l)::tail => if(pt==t) d::dresseurByPokOfThisType(tail,t) else dresseurByPokOfThisType(tail,t)
			case _ => Nil
		}
	}


	/* ----------------------------------- Combats ----------------------------------- */
	def combat(p:List[Pokemon]): Pokemon = {
		val rp = Random.shuffle(p)
		if(rp(0).level>rp(1).level)
			rp(0)
		else
			rp(1)
	}

	def highPokemonLevel(p1:Pokemon, p2:Pokemon): Pokemon = {
		if(p1.level>p2.level)
			p1
		else
			p2
	}

	def longPokemonName(p1:Pokemon, p2:Pokemon): Pokemon = {
		if(p1.name>p2.name)
			p1
		else
			p2
	}

	def numberOfPokByDresseur(p:List[Pokemon], d:Dresseur): Int = {
		p match {
			case Pokemon(n,t,e,Some(dr),l)::tail if(dr==d) => 1 + numberOfPokByDresseur(tail,d)
			case Pokemon(_,_,_,_,_)::tail => numberOfPokByDresseur(tail,d)
			case Nil => 0
		}
	}

	def mostPokemonNumber(d1:Dresseur, d2:Dresseur): Dresseur = {
		if(numberOfPokByDresseur(p,d1) > numberOfPokByDresseur(p,d2))
			d1
		else
			d2
	}

	def combatPolymorph[A](l:List[A], f:(A,A)=>A): A = {
		val rl = Random.shuffle(l)
		f(rl(0),rl(1))
	}

	def main(args: Array[String]) {

		/* Affichage et vérification de l'échauffement */
		//pokHigherThan(p,12).map(println)
		//pokWithDresseur(p).map(println)
		//allEvolution(p).map(println)
		//pokByDresseurAge(p,14).map(println)
		//pokByDresseurName(p,'S').map(println)
		//println(meanDresseurAge(d))
		//println(pokMinMaxLvl(p))
		//dresseurByPokOfThisType(p,"electrique").map(println)

		/* Affichage et vérification du combat */
		println(combat(p))
		println(combatPolymorph(p,highPokemonLevel))
		println(combatPolymorph(p,longPokemonName))
		println(combatPolymorph(d,mostPokemonNumber))

	}

}
