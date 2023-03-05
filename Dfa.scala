class Dfa[A] (var Alfabet: Set[Char],
              var Nr_Stari: Int,
              var Stari: Set[A],
              var Stare_Initiala: A,
              var Stari_Finale: Set[A],
              var Tranzitii: Map[(A, Char), A]
             )
{

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    new Dfa[B](Alfabet, Nr_Stari, Stari.map(f), f(Stare_Initiala), Stari_Finale.map(f),
      Tranzitii.map(tranzitie => ((f(tranzitie._1._1), tranzitie._1._2), f(tranzitie._2))))
  }
      def next(state: A, c: Char): A = {
        if (Tranzitii.contains((state, c))) {
      Tranzitii((state, c))
    } else {
      state
    }
  }

  def checkIfAccepted(str: String, state: A): Boolean = {
    if (str.isEmpty) {
      isFinal(state)
    } else if (!Tranzitii.contains((state, str.charAt(0)))) {
      false
    } else {
      checkIfAccepted(str.drop(1), Tranzitii((state, str.charAt(0))))
    }
  }

  def accepts(str: String): Boolean = {
    val ok: Boolean = str.forall(c => Alfabet.contains(c))
    if (!ok) {
      false
    } else {
      checkIfAccepted(str, Stare_Initiala)
    }
  }

  def getStates : Set[A] = {
    Stari
  }

  def isFinal(state: A): Boolean = {
    Stari_Finale.contains(state)
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  def dfaFromNfa(nfa: Nfa[Int]): Dfa[Set[Int]] = {
    var stari: List[Set[Int]] = List()
    var tranzitii: Map[(Set[Int], Char), Set[Int]] = Map()
    var stare_Initiala: Set[Int] = Set()

    stari ++= List(nfa.getEpsClosure(nfa.Stare_Initiala))
    stare_Initiala = stari.head

    var index : Int = 0

    while(index < stari.length) {
      for (c <- nfa.Alfabet) {
        var newState: Set[Int] = Set()
        for (stare <- stari(index)) {
          newState ++= nfa.nextAndEpsClosure(stare, c)
        }
        if (newState.nonEmpty && !stari.contains(newState)) {
          stari ++= List(newState)
        }
        if (newState.nonEmpty) {
          tranzitii ++= Map((stari(index), c) -> newState)
        }
      }
      index = index + 1
    }

    new Dfa[Set[Int]](nfa.Alfabet, stari.toSet.size, stari.toSet, stare_Initiala,
      stari.toSet.filter(stare => stare.contains(nfa.Stari_Finale.head)), tranzitii)
  }

  def modifyStates(dfa: Dfa[Set[Int]]): Dfa[Int] = {
    val states : List[Set[Int]] = dfa.Stari.toList
    val tranzitii : Map[(Int, Char), Int] =
      dfa.Tranzitii.map(tranzitie => ((states.indexOf(tranzitie._1._1), tranzitie._1._2), states.indexOf(tranzitie._2)))
    new Dfa[Int](dfa.Alfabet, states.length, states.map(stare => states.indexOf(stare)).toSet, states.indexOf(dfa.Stare_Initiala),
      dfa.Stari_Finale.map(stare => states.indexOf(stare)), tranzitii)
  }

  def fromPrenex(str: String): Dfa[Int] = {
    modifyStates(dfaFromNfa(Nfa.fromPrenex(str)))
  }
  // You can add more methods to this object
}
