import api.Writing._
import api.Reading._

object Hero {

	abstract class Character {
		def name: String
	}

	case class Civilian(name: String, wealth: Int) extends Character
	case class SuperHero(name: String, powers: List[String], alterEgo: Option[Civilian]) extends Character
	case class Enemy(name: String, archEnemy: SuperHero) extends Character

	val TonyStark = Civilian("Tony Stark", 1000000)
	val BruceWayne = Civilian("Bruce Wayne", 1000000)
	val ClarkKent = Civilian("Clark Kent", 1000)
	val IronMan = SuperHero("Iron Man", List("SuperhumanStrength", "Genius", "Cyborg"), Some(TonyStark))
	val Batman = SuperHero("Batman", List("Genius", "Gadgets"), Some(BruceWayne))
	val Superman = SuperHero("SuperMan", List("SuperhumanStrength", "Invulnerability"), Some(ClarkKent))
	val Wolverine = SuperHero("Wolverine", List("SuperhumanStrength", "Invulnerability"), None)
	val Joker = Enemy("Joker",Batman)

	// Quels sont les super-héros qui sont invulnérables ?
	def invulnerable(p: List[SuperHero]): List[String] = {
		p match {
			case SuperHero(name,powers,_) :: tail => if(powers.contains("Invulnerability")) name :: invulnerable(tail) else invulnerable(tail)
			case _ => Nil
		}
	}

	// Quels sont les super-héros qui ont au moins 2 super-pouvoirs ?
	def pouvoirs(p: List[SuperHero]): List[String] = {
		p match {
			case SuperHero(name,powers,_) :: tail => if(powers.length>=2) name :: pouvoirs(tail) else pouvoirs(tail)
			case _ => Nil
		}
	}

	// Quel super-héros n'a pas d'alter-égo ?
	def notHaveAlterEgo(p: List[SuperHero]): List[String] = {
		p match {
			case SuperHero(_,_,Some(_)) :: tail => notHaveAlterEgo(tail)
			case SuperHero(name,_,None) :: tail => name :: notHaveAlterEgo(tail)
			case _ => Nil
		}
	}

	// Quels sont les civils qui gagnent moins de 10000 euros et dont l'alter-égo est invulnérable ?
	def vasi(p: List[SuperHero]): List[String] = {
		p match {
			case SuperHero(_,powers,Some(Civilian(name,wealth))) :: tail => if(powers.contains("Invulnerability") && wealth<10000) name :: vasi(tail) else vasi(tail)
			case SuperHero(_,_,_) :: tail => vasi(tail)
			case _ => Nil
		}
	}

	// Calculez la fortune de tous vos personnages réunis.
	def fortune(p: List[Character]): String = {
		def f(p: List[Character], acc:Int): String = {
			p match {
				case Civilian(_,wealth) :: tail => f(tail,acc+wealth)
				case _ :: tail => f(tail,acc)
				case _ => acc.toString
			}
		}	
		f(p,0)
	}

	// Listez tous les super-pouvoirs possibles.
	def tousLesPouvoirs(p: List[SuperHero]): List[String] = {
		p match {
			case SuperHero(_,powers,_) :: tail => powers ::: tousLesPouvoirs(tail)
			case _ => Nil
		}
	}

	// Listez tous les noms (super-héros, civils, ennemis).
	def tousLesNoms(personne: List[Character]): List[String] = {
		personne match {
			case Civilian(name,_) :: tail => name :: tousLesNoms(tail)
			case SuperHero(name,_,_) :: tail => name :: tousLesNoms(tail)
			case Enemy(name,_) :: tail => name :: tousLesNoms(tail)
			case _ :: tail => tousLesNoms(tail)
			case _ => Nil
		}
	}

	val l = List(IronMan,Batman,Superman,Wolverine)
	val l2 = List(TonyStark,BruceWayne,ClarkKent,IronMan,Batman,Superman,Wolverine)

	/*def createCivilian(l: List[Character]): List[Character] = {
		println("Nom du civil ?")
		val n = Console.readLine
		println("Fortune du civil ?")
		val w = Console.readLine
		Civilian(n,w)::l
	}

	def creation(l: List[Character]): List[Character] = {
		println("Choisir :")
		println("0 - Creer un civil")
		println("1 - Supprimer un civil")
		println("2 - Creer un superhero")
		println("3 - Supprimer un superhero")
		println("4 - Creer un enemy")
		println("5 - Supprimer un enemy")
		println("6 - Revenir au menu")
		val line = Console.readLine
		line match {
			case "0" => val l=createCivilian(l);creation(l)
			case "1" => creation(l)
			case "2" => creation(l)
			case "3" => creation(l)
			case "4" => creation(l)
			case "5" => creation(l)
			case "6" => creation(l)
			case _ => println("Faites un choix valide");creation(l)
		}
		return l
	}

	def menu(l: List[Character]): List[Character] = {
		println("Chosir :")
		println("0 - Quitter")
		println("1 - Quels sont les super-héros qui sont invulnérables ?")
		println("2 - Quels sont les super-héros qui ont au moins 2 super-pouvoirs ?")
		println("3 - Quel super-héros n'a pas d'alter-égo ?")
		println("4 - Quels sont les civils qui gagnent moins de 10000 euros et dont l'alter-égo est invulnérable ?")
		println("5 - Calculez la fortune de tous vos personnages réunis.")
		println("6 - Listez tous les super-pouvoirs possibles.")
		println("7 - Listez tous les noms (super-héros, civils, ennemis).")
		println("8 - Creation et suppression de personnages")
		val line = Console.readLine
		line match {
			case "0" => println("Bye");System.exit(1)
			case "1" => invulnerable(l).map(println);menu(l)
			case "2" => pouvoirs(l).map(println);menu(l)
			case "3" => notHaveAlterEgo(l).map(println);menu(l)
			case "4" => vasi(l).map(println);menu(l)
			case "5" => fortune(l).map(println);menu(l)
			case "6" => tousLesPouvoirs(l).map(println);menu(l)
			case "7" => tousLesNoms(l).map(println);menu(l)
			case "8" => val l = creation(l);
			case _ => println("Faites un choix valide");menu(l)
		}
		return l
	}*/

	def main(args: Array[String]) {

		/*
		invulnerable(l).map(println)
		pouvoirs(l).map(println)
		notHaveAlterEgo(l).map(println)
		vasi(l).map(println)
		println(fortune(l2))
		tousLesPouvoirs(l).map(println)
		tousLesNoms(l2).map(println)
		*/

		//menu()

		val t = List(TonyStark,BruceWayne,ClarkKent).foldLeft(List[String]()) { (z,f) =>
			z :+ f.name+";"+f.wealth
		}
		writeCivil("civilian.csv",t)
		read("civilian.csv")

		List(IronMan,Batman,Superman,Wolverine).map(println)
		val t2 = List(IronMan,Batman,Superman,Wolverine).foldLeft(List[String]()) { (z,f) =>
			z :+ f.name+";"+f.powers.mkString(" ")+";"+f.alterEgo.map(v => v.name).getOrElse("Nan")
		}
		writeCivil("superHero.csv",t2)

		println("-------------")

		val s = read("superHero.csv").mkString("/")
		println(s)

		val t3 = List(Joker).foldLeft(List[String]()) { (z,f) =>
				z :+ f.name+";"+f.archEnemy.name
		}
		writeCivil("enemy.csv",t3)
		read("enemy.csv")
	}

}
