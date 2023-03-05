import scala.annotation.tailrec

class Nfa[A](var Alfabet: Set[Char],
             var Nr_Stari: Int,
             var Stari: Set[A],
             var Stare_Initiala: A,
             var Stari_Finale: Set[A],
             var Tranzitii: Map[(A, String), Set[A]]
            )
{
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    new Nfa[B](Alfabet, Nr_Stari, Stari.map(f), f(Stare_Initiala), Stari_Finale.map(f),
      Tranzitii.map(tranzitie => ((f(tranzitie._1._1), tranzitie._1._2), tranzitie._2.map(f))))
  }

  def next(state:A, c: Char): Set[A] = {
    if (!Tranzitii.contains((state, c.toString))) {
      Set()
    } else {
      Tranzitii((state, c.toString))
    }
  }

  def nextAndEpsClosure(state: A, c: Char): Set[A] = {
    var states : List[A] = next(state, c).toList
    var index : Int = 0
    val lenInit : Int = states.length
    while (index < lenInit) {
      states ++= getEpsClosure(states(index))
      index = index + 1
    }
    states.toSet
  }

  def checkIfAccepted(str: String, state: A, steps: Int): Boolean = {
    if(str.isEmpty && isFinal(state)) {
      return true
    }
    if (steps > 50) {
      return false
    }
    var ok: Boolean = false
    if (str.nonEmpty && Tranzitii.contains(state, str.charAt(0).toString)) {
      Tranzitii((state, str.charAt(0).toString)).foreach((stare: A) => ok = ok || checkIfAccepted(str.drop(1), stare, 0))
    }
    if (Tranzitii.contains(state, "eps")) {
      Tranzitii((state, "eps")).foreach((stare: A) => ok = ok || checkIfAccepted(str, stare, steps + 1))
    }
    ok
  }

  def accepts(str: String): Boolean = {
    val ok : Boolean = str.forall(c => Alfabet.contains(c))
    if (!ok) {
      false
    } else {
    checkIfAccepted(str, Stare_Initiala, 0)
    }
  }

  def getStates : Set[A] = {
    Stari
  }

  def isFinal(state: A): Boolean = {
    Stari_Finale.contains(state)
  }

  def epsClosure(state: A, states: Set[A]): Set[A] = {
    if (states.contains(state)) {
      states
    } else {
      var stari1 : Set[A] = states + state
      if (Tranzitii.contains((state, "eps"))) {
        Tranzitii((state, "eps")).foreach(stare => stari1 = epsClosure(stare, stari1))
      }
      stari1
    }
  }

  def getEpsClosure(state: A) : Set[A] = {
    epsClosure(state, Set())
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class

object Nfa {

  class regexTree(var root: String, var left: regexTree, var right: regexTree) {
  }

  def addSpaces(elemente: List[String]) : List[String] = {
    if (elemente.isEmpty)
      List()
    else if (elemente.length == 1) {
      elemente
    } else if (elemente.head == "'" && elemente(1) == "'") {
      List(" ") ++ addSpaces(elemente.drop(2))
    } else {
      List(elemente.head) ++ addSpaces(elemente.drop(1))
    }
  }

  def prenexModif(elemente: String) : List[String] = {
    if (addSpaces(elemente.split(" ").toList).isEmpty) {
      List(" ")
    } else {
      addSpaces(elemente.split(" ").toList)
    }
  }

  def nfaFromCharacter(car: String): Nfa[Int] = {
    if (car == "eps") {
      new Nfa[Int](Set(), 1, Set(0), 0, Set(0), Map())
    } else if (car == "void") {
      new Nfa[Int](Set(), 2, Set(0, 1), 0, Set(1), Map())
    } else {
      new Nfa[Int](Set(car(0)), 2, Set(0, 1), 0, Set(1), Map((0, car(0).toString) -> Set(1)))
    }
  }

  def modifTranzitie1(tranzitie: ((Int, String), Set[Int])): ((Int, String), Set[Int]) = {
    ((tranzitie._1._1 + 1, tranzitie._1._2), tranzitie._2.map(stare => stare + 1))
  }

  def modifTranzitie2(tranzitie: ((Int, String), Set[Int]), nr: Int): ((Int, String), Set[Int]) = {
    ((tranzitie._1._1 + nr + 1, tranzitie._1._2), tranzitie._2.map(stare => stare + nr + 1))
  }

  def nfaFromUnion(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    new Nfa[Int](nfa1.Alfabet ++ nfa2.Alfabet, nfa1.Nr_Stari + nfa2.Nr_Stari + 2,
      nfa1.Stari.map(stare => stare + 1) ++ nfa2.Stari.map(stare => stare + nfa1.Nr_Stari + 1) ++
        Set(0, nfa1.Nr_Stari + nfa2.Nr_Stari + 1), 0,
        Set(nfa1.Nr_Stari + nfa2.Nr_Stari + 1),
      nfa1.Tranzitii.map(tranzitie => modifTranzitie1(tranzitie)) ++ nfa2.Tranzitii.map(tranzitie => modifTranzitie2(tranzitie, nfa1.Nr_Stari)) ++
        Map((0, "eps") -> Set(nfa1.Stare_Initiala + 1, nfa2.Stare_Initiala + nfa1.Nr_Stari + 1),
          (nfa1.Stari_Finale.head + 1, "eps") -> Set(nfa1.Nr_Stari + nfa2.Nr_Stari + 1),
          (nfa2.Stari_Finale.head + nfa1.Nr_Stari + 1, "eps") -> Set(nfa1.Nr_Stari + nfa2.Nr_Stari + 1)))
  }

  def nfaFromConcat(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
    new Nfa[Int](nfa1.Alfabet ++ nfa2.Alfabet, nfa1.Nr_Stari + nfa2.Nr_Stari,
      nfa1.Stari ++ nfa2.Stari.map(stare => stare + nfa1.Nr_Stari), 0, Set(nfa1.Nr_Stari + nfa2.Nr_Stari - 1),
      nfa1.Tranzitii ++ nfa2.Tranzitii.map(tranzitie => modifTranzitie2(tranzitie, nfa1.Nr_Stari - 1)) ++
        Map((nfa1.Stari_Finale.head, "eps") -> Set(nfa2.Stare_Initiala + nfa1.Nr_Stari)))
  }

  def nfaFromStar(nfa: Nfa[Int]): Nfa[Int] = {
    new Nfa[Int](nfa.Alfabet, nfa.Nr_Stari + 2, nfa.Stari.map(stare => stare + 1) ++ Set(0, nfa.Nr_Stari + 1), 0,
      Set(nfa.Nr_Stari + 1), nfa.Tranzitii.map(tranzitie => modifTranzitie1(tranzitie)) ++ Map((0, "eps") -> Set(nfa.Stare_Initiala + 1, nfa.Nr_Stari + 1),
        (nfa.Stari_Finale.head + 1, "eps") -> Set(nfa.Stare_Initiala + 1, nfa.Nr_Stari + 1)))
  }

  def isAtom(str: String) : Boolean = {
    if (str.length == 1 || str.charAt(0) == '\'' || str == "void" || str == "eps")
      true
    else {
      false
    }
  }

  def isolateCharacter(str: String): String = {
    if (str.length > 1 && str.charAt(0) == '\'' ) {
      str.charAt(1).toString
    } else {
      str
    }
  }

  def jumpIncrease(str: String) : Int = {
    if (str == "CONCAT" || str == "UNION") {
      2
    } else if (str == "STAR" || str == "PLUS" || str == "MAYBE") {
      1
    } else {
      0
    }
  }

  @tailrec
  def countJump(elemente: List[String], index: Int, count: Int): Int = {
    if (index + 1 == count) {
      count
    } else {
      countJump(elemente.drop(1), index + 1, count + jumpIncrease(elemente.head))
    }
  }

  def getTreeWithPlusAndMaybe(elemente: List[String]): regexTree = {
    if (isAtom(elemente.head)) {
      new regexTree(isolateCharacter(elemente.head), null, null)
    } else if (elemente.head == "STAR" || elemente.head == "PLUS" || elemente.head == "MAYBE") {
      new regexTree(elemente.head, getTreeWithPlusAndMaybe(elemente.drop(1)), null)
    } else if (elemente.head == "CONCAT" || elemente.head == "UNION") {
      new regexTree(elemente.head, getTreeWithPlusAndMaybe(elemente.drop(1)), getTreeWithPlusAndMaybe(elemente.drop(countJump(elemente.drop(1), 0, 2))))
    } else {
      new regexTree(null, null, null)
    }
  }

  def treeRemovePlusAndMaybe(tree: regexTree): regexTree = {
    if (tree == null) {
      null
    } else if (isAtom(tree.root)) {
      new regexTree(tree.root, null, null)
    } else if (tree.root == "PLUS") {
      new regexTree("CONCAT", treeRemovePlusAndMaybe(tree.left), new regexTree("STAR", treeRemovePlusAndMaybe(tree.left), null))
    } else if (tree.root == "MAYBE") {
      new regexTree("UNION", treeRemovePlusAndMaybe(tree.left), new regexTree("eps", null, null))
    } else {
      new regexTree(tree.root, treeRemovePlusAndMaybe(tree.left), treeRemovePlusAndMaybe(tree.right))
    }
  }

  def getTree(elemente: String): regexTree = {
    treeRemovePlusAndMaybe(getTreeWithPlusAndMaybe(prenexModif(elemente)))
  }

  def printTree1(tree: regexTree): Unit = {
    if (isAtom(tree.root)) {
      print(tree.root)
    } else if (tree.root == "STAR" || tree.root == "PLUS" || tree.root == "MAYBE") {
      print(tree.root)
      print(" ( ")
      printTree1(tree.left)
      print(" )")
    } else if (tree.root == "CONCAT" || tree.root == "UNION") {
      print(tree.root)
      print(" ( ")
      printTree1(tree.left)
      print("  ")
      printTree1(tree.right)
      print(" )")
    }
  }

  def treeToPrenex(tree: regexTree): String = {
    if (isAtom(tree.root)) {
      if (tree.root == " ") {
        "\'" ++ tree.root ++ "\'"
      } else {
        tree.root
      }
    } else if (tree.root == "STAR" || tree.root == "PLUS" || tree.root == "MAYBE") {
      tree.root ++ " " ++ treeToPrenex(tree.left)
    } else if (tree.root == "CONCAT" || tree.root == "UNION") {
      tree.root ++ " " ++ treeToPrenex(tree.left) ++ " " ++ treeToPrenex(tree.right)
    } else {
      ""
    }
  }

  def printTree(tree: regexTree): Unit = {
    printTree1(tree)
    println()
  }

  def nfaFromTree(tree: regexTree) : Nfa[Int] = {
      if (isAtom(tree.root)) {
        nfaFromCharacter(tree.root)
      } else if (tree.root == "STAR") {
        nfaFromStar(nfaFromTree(tree.left))
      } else if (tree.root == "UNION") {
        nfaFromUnion(nfaFromTree(tree.left), nfaFromTree(tree.right))
      } else if (tree.root == "CONCAT") {
        nfaFromConcat(nfaFromTree(tree.left), nfaFromTree(tree.right))
      } else {
        nfaFromCharacter("void")
      }
  }

  def fromPrenex(str: String): Nfa[Int] = {
    nfaFromTree(getTree(str))
  }

  // You can add more methods to this object
}