object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */

  class MyCharacter(var valoare: String) {
    def print1(): Unit = {
      print(valoare)
    }
    def print2(): Unit = {
      print("chr ")
    }
  }

  class MyControl(var valoare: String) {
    def print1(): Unit = {
      print(valoare)
    }
    def print2(): Unit = {
      print("ctr ")
    }
  }

  def getTypes(s: List[Char]): List[Either[MyCharacter, MyControl]] = {
    var i : Int = 0
    var elemente : List[Either[MyCharacter, MyControl]] = List()
    while (i < s.length) {
      if (s(i) == '(' || s(i) == ')' || s(i) == '?' || s(i) == '*' || s(i) == '|' || s(i) == '+') {
        elemente = elemente ++ List(Right(new MyControl(s(i).toString)))
        i = i + 1
      } else if (s(i) == '\'') {
        i = i + 1
        if (s(i + 1) == '\'') {
          elemente = elemente ++ List(Left(new MyCharacter(s(i).toString)))
          i = i + 2
        } else {
          elemente = elemente ++ List(Left(new MyCharacter(s.toString().substring(i, i + 1))))
          i = i + 3
        }
      } else if (i + 2 < s.length && s(i) == 'e' && s(i + 1) == 'p' && s(i + 2) == 's') {
        elemente = elemente ++ List(Left(new MyCharacter("eps")))
        i = i + 3
      } else {
        elemente = elemente ++ List(Left(new MyCharacter(s(i).toString)))
        i = i + 1
      }
    }
    elemente
  }

  def addParenthesis(s: List[Either[Regex.MyCharacter, Regex.MyControl]]) : List[Either[Regex.MyCharacter, Regex.MyControl]] = {
    var i: Int = 0
    var inchise: Int = 0
    var deschise: Int = 0
    var regex: List[Either[Regex.MyCharacter, Regex.MyControl]] = s

    while (i < regex.length) {
      if (getType(regex, i) == "ctr" && getValue(regex, i) == "(") {
        deschise = deschise + 1
      } else if (getType(regex, i) == "ctr" && getValue(regex, i) == ")") {
        inchise = inchise + 1
      } else if (getType(regex, i) == "ctr" && getValue(regex, i) == "|") {
        regex = regex.take(i + 1) ++ List(Right(new MyControl("("))) ++ regex.drop(i + 1)
        i = i + 1
        var j: Int = i + 1
        var deschiseAux: Int = deschise
        var inchiseAux: Int = inchise
        while (j < regex.length && deschiseAux - inchiseAux + 1 != deschise - inchise) {
          if (getType(regex, j) == "ctr" && getValue(regex, j) == "(") {
            deschiseAux = deschiseAux + 1
          } else if (getType(regex, j) == "ctr" && getValue(regex, j) == ")") {
            inchiseAux = inchiseAux + 1
          }
          j = j + 1
        }
        regex = regex.take(j) ++ List(Right(new MyControl(")"))) ++ regex.drop(j)
      }
      i = i + 1
    }
    regex
  }

  def preprocess(s:List[Char]): List[Either[MyCharacter, MyControl]] = {
    addParenthesis(getTypes(s.mkString.replace("[0-9]", "(0|1|2|3|4|5|6|7|8|9)")
      .replace("[a-z]","(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
      .replace("[A-Z]","(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)").toList))
  }

  def printEither1(s: List[Either[MyCharacter, MyControl]]): Unit = {
    s.foreach {
      case Right(x) => x.print1()
      case Left(x) => x.print1()
    }
    println()
  }

  def getValue(s: List[Either[MyCharacter, MyControl]], index: Int): String = {
    s(index) match {
      case Right(x) => x.valoare
      case Left(x) => x.valoare
    }
  }

  def getType(s: List[Either[MyCharacter, MyControl]], index: Int): String = {
    s(index) match {
      case Right(_) => "ctr"
      case Left(_) => "chr"
    }
  }

  def printEither2(s: List[Either[MyCharacter, MyControl]]): Unit = {
    s.foreach {
      case Right(x) => x.print2()
      case Left(x) => x.print2()
    }
    println()
  }

  def count(s: List[Either[MyCharacter, MyControl]]): Int = {
    var i: Int = s.length - 1

    while ((getType(s, i) == "ctr" && getValue(s, i) != ")") && getType(s, i) != "chr") {
      i = i - 1
    }
    if (getType(s, i) == "chr") {
      s.length - i
    } else {
      var nrDeschise: Int = 0
      var nrInchise: Int = 1
      i = i - 1
      while (nrInchise > nrDeschise) {
        if (getType(s, i) == "ctr") {
          if (getValue(s, i) == "(") {
            nrDeschise = nrDeschise + 1
          } else if (getValue(s, i) == ")") {
            nrInchise = nrInchise + 1
          }
        }
        i = i - 1
      }
      s.length - i - 1
    }
  }

  def treeFromRegex(s: List[Either[MyCharacter, MyControl]]): Nfa.regexTree = {
    val l = s.length
    var unary: Int = 0
    var unaryValue: String = ""
    if (getType(s, l - 1) == "ctr") {
      if (getValue(s, l - 1) == "*" || getValue(s, l - 1) == "?" || getValue(s, l - 1) == "+") {
        unary = 1
        getValue(s, l - 1) match {
          case "*" => unaryValue = "STAR"
          case "?" => unaryValue = "MAYBE"
          case "+" => unaryValue = "PLUS"
        }
      }
    }
    val subRegexLength: Int = count(s.dropRight(unary))
    if (subRegexLength + unary == l) {
      if (subRegexLength == 1) {
        if (unary == 1) {
          new Nfa.regexTree(unaryValue, new Nfa.regexTree(getValue(s, 0), null, null), null)
        } else {
          new Nfa.regexTree(getValue(s, 0), null, null)
        }
      } else if (getValue(s, l - 1 - unary) != ")") {
        if (unary == 1) {
          new Nfa.regexTree(unaryValue, treeFromRegex(s.dropRight(1)), null)
        } else {
          treeFromRegex(s.dropRight(1))
        }
      } else {
        if (unary == 1) {
          new Nfa.regexTree(unaryValue, treeFromRegex(s.dropRight(2).drop(1)), null)
        } else {
          treeFromRegex(s.dropRight(1).drop(1))
        }
      }
    } else {
      var nextOp: String = ""
      var union: Int = 0
      if (getType(s, l - 1 - unary - subRegexLength) == "ctr" && getValue(s, l - 1 - unary - subRegexLength) == "|") {
        nextOp = "UNION"
        union = 1
      } else {
        nextOp = "CONCAT"
      }
      var leftTree: Nfa.regexTree = new Nfa.regexTree(null, null, null)
      if (unary == 1) {
        leftTree = new Nfa.regexTree(unaryValue, treeFromRegex(s.dropRight(unary).drop(l - unary - subRegexLength)), null)
      } else {
        leftTree = treeFromRegex(s.dropRight(unary).drop(l - unary - subRegexLength))
      }
      new Nfa.regexTree(nextOp, treeFromRegex(s.dropRight(subRegexLength + unary + union)), leftTree)
    }
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    Nfa.treeToPrenex(treeFromRegex(preprocess(str.toList)))
  }
}
