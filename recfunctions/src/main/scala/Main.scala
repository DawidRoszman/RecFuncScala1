// PATENT NA EX-AEQUO
  // val klasyfikacja = wyniki
  // .map(w => Wynik(wyniki.filter(el => (el.suma > w.suma) || (el.suma == w.suma && el.średniWdzięk > w.średniWdzięk)).length + 1, w.imię, w.nazwisko, w.średniWdzięk, w.średniSpryt, w.suma))

// isPrime
def isPrime(n: Int): Boolean = {
  require(n >= 2)

  @annotation.tailrec
  def go(a: Int, b: Int): Boolean = {
    b match {
      case 1 => true
      case _ if a%b == 0 => false
      case _ if a%b != 0 => go(a, b/2)
    }
  }
  go(n, n/2)
}

// reverseString
def reverseString(napis: String): String = {
    @annotation.tailrec
    def go(lista: List[String], aku: List[String]): String = {
        lista match {
            case Nil => aku.mkString("")
            case head :: next => go(next, head :: aku)
            case null => "twoj stary"
        }
    }
    go(napis.split("").toList,Nil) //split() zwraca Array a nie List
}

// intToBin
def intToBin(liczba: Int):Int = {
    @annotation.tailrec
    def go(liczba: Int, acc: String): String = {
        liczba match {
            case 0 => acc
            case _ if liczba%2 == 1 => go(liczba/2, acc + "1")
            case _ if liczba%2 == 0 => go(liczba/2, acc + "0")
        }
    }
    reverseString(go(liczba, "")).toInt
}

// last
def last[A](list: List[A]): A = {
    def go(list: List[A]): A = {
        list match {
            case head :: Nil => head
            case head :: tail => go(tail)
            case Nil => throw new Exception("Nie możesz podawać pustej listy")
        }
    }
    go(list)
}

// compress
def compress(list: List[Int]): List[Int] = {
    def go(list: List[Int], aku: List[Int], lastElement: Int): List[Int] = {
        list match{
            case Nil => reverseList(aku)
            case head :: next =>  if lastElement == head then go(next, aku, lastElement) else go(next, head :: aku, head)
        }
    }
    go(list, Nil, -2137)
}

// to samo inaczej deStutter
def deStutter[A](list: List[A]): List[A] = {
  reverseList(foldLeft(list)(List[A]()) { (acc, curr) => acc match {
    case Nil => curr :: acc
    case head :: _ => if (curr != head) curr :: acc else acc
    }
  })
}

// reverseList
def reverseList[A](list: List[A]): List[A] = {
  def go[A](list: List[A], acc: List[A]): List[A]={
      list match {
        case Nil => acc
        case head :: next => go(next, head :: acc)
      }
  }
  go(list, Nil)
}

// take
def take[A](list: List[A], n: Int): List[A] ={
  def go[A](list: List[A], n: Int, acc : List[A]): List[A] = {
      (n, list) match
        case (0, _) => acc
        case (_, head::tail) => go(tail, n-1, head::acc)
        case (_, _) => acc


  }
  reverseList(go(list, n, Nil))
}

// drop
def drop[A](list: List[A], n: Int): List[A] = {
  def go[A](list: List[A], n: Int): List[A] = {
    (n, list) match {
      case (0, _) => list
      case (_, head::tail) => go(tail, n-1)
      case (_, _) => Nil
    }
  }
  go(list, n)
}

// length
def length[A](list: List[A]): Int = {
  def go[A](list: List[A], acc: Int): Int = {
    list match {
      case Nil => acc
      case head::tail => go(tail, acc+1)
    }
  }
  go(list, 0)
}

// map
@annotation.tailrec
def map[A, B](arr: List[A], func: A => B, acc: List[B] = List()): List[B] = arr match {
  case head :: tail => map(tail, func, func(head) :: acc)
  case Nil => reverseList(acc)
}

// flatten
def flatten(list: List[Any]): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match{
        case Nil => reverseList(aku)
        case (head : List[Any]) :: tail => go(concat(head, tail), aku)
        case head :: tail => go(tail, head :: aku)
      }
  }
  go(list, Nil)
}

// flatMap
def flatMap(list: List[Any], fun: Any => Any): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match {
          case Nil => reverseList(aku)
          case head::tail => go(tail,fun(head)::aku)
      }
  }
  flatten(go(list,Nil))
}

// flatMap2
@annotation.tailrec
def flatMap2[A, B](arr: List[A], func: A => List[B], acc: List[B] = List()): List[B] = arr match {
  case head :: tail => flatMap2(tail, func, concat(reverseList(func(head)), acc))
  case Nil => reverseList(acc)
}

// dropKthElem
def dropKthElem[A](list: List[A], k: Int): List[A] = {
  def go(list: List[A], ind: Int, acc: List[A]): List[A] = {
    list match{
      case Nil => reverseList(acc)
      case head :: next if ind == k => go(next, ind + 1, acc)
      case head :: next  => go(next, ind + 1, head :: acc)
    }
  }
  go(list,0,Nil)
}

// filter
def filter[A](list: List[A],f: A=>Boolean): List[A] = {

    def go(list: List[A], aku: List[A]): List[A] = {
      list match{
        case Nil => reverseList(aku)
        case head :: next => f(head) match{
          case true => go(next, head::aku)
          case false => go(next, aku)
        }
      }
    }
    go(list, Nil)
}

// split
def split(str: String, delimiter: String): List[String] = {
  @annotation.tailrec
  def strLoop(strLeft: List[Char], curString: String = "", list: List[String] = List()): List[String] = strLeft match {
    case head :: tail => {
      if (head.toString == delimiter && curString != "") strLoop(tail, "", curString :: list)
      else if (head.toString != delimiter) strLoop(tail, curString + head, list)
      else strLoop(tail, curString, list)
    }
    case Nil => if (curString != "") curString :: list else list
  }
  reverseList(strLoop(str.toList))
}

// max
def max(arr: List[Int]): Int = {
  @annotation.tailrec
  def maxLoop(arrL: List[Int], currMax: Option[Int] = None): Option[Int] = (arrL, currMax) match {
    case (head :: _, None) => maxLoop(arrL, Some(head))
    case (head :: tail, Some(value)) => if head > value then maxLoop(tail, Some(head)) else maxLoop(tail, Some(value))
    case (Nil, Some(value)) => Some(value)
    case (Nil, None) => None
  }
  val res = maxLoop(arr)
  res match {
    case None => 0
    case Some(value) => value
  }
}

// foldLeft
@annotation.tailrec
def foldLeft[A, B](list: List[A])(acc: B)(op: (B, A) => B): B = list match {
  case head :: tail => foldLeft(tail)(op(acc, head))(op: (B, A) => B)
  case _ => acc
}

// concat
@annotation.tailrec
def concat[B](arr1: List[B], arr2: List[B], acc: List[B]=List()): List[B] = arr1 match {
  case head :: tail => concat(tail, arr2, head :: acc)
  case Nil => arr2 match {
    case head :: tail => concat(arr1, tail, head :: acc)
    case Nil => reverseList(acc)
  }
}

// sliding (relies on drop)
def sliding[A](list: List[A])(len: Int, shift: Int = 1): List[List[A]] = {
  @annotation.tailrec
  def slidLoop[A](list: List[A], curList: List[A])(len: Int, shift: Int, curLen: Int, res: List[A]=Nil, acc: List[List[A]]=Nil): List[List[A]] = curList match {
    case head :: tail if (curLen > 0) => slidLoop(list, tail)(len, shift, curLen - 1, head :: res, acc)
    case head :: tail if (curLen == 0) => slidLoop(drop(list, shift), drop(list, shift))(len, shift, len, Nil, reverseList(res) :: acc)
    case _ if (res != Nil) => reverseList(reverseList(res) :: acc)
    case _ => reverseList(acc)
  }

  slidLoop(list, list)(len, shift, len)
}

// Wyciąganie elementu z listy o konkretnym indexie
// getElementByIndex
def getElementByIndex[A](list: List[A], index: Int): A = {
    list match {
        case head :: tail if (index <= 0) => head
        case head :: tail => getElementByIndex(tail, index - 1)
        case _ => throw new Exception("----------- GET_ELEMENT_BY_INDEX IS SAYING: THIS INDEX DOES NOT EXISTS -----------")
    }
}

// Insert elementu do listy, potrzebuje funkcji myReverse
// insertElementInIndex
def insertElementInIndex[A](list: List[A], el: A, index: Int): List[A] = {
    def insertElementInIndexHelp(list: List[A], el: A, index: Int, acc: List[A] = List(), isInserted: Boolean = false): List[A] = list match {
        case head :: tail if (index <= 0 && isInserted) => insertElementInIndexHelp(tail, el, index, head :: acc, isInserted)
        case head :: tail if (index <= 0) => insertElementInIndexHelp(list, el, index, el :: acc, true)
        case head :: tail => insertElementInIndexHelp(tail, el, index - 1, head :: acc)
        case _ if !(isInserted) => reverseList(el :: acc)
        case _ => reverseList(acc)
    }
    insertElementInIndexHelp(list, el, index)
}

// Sortowanie przez wybór (tylko listy o podobnych i porównywalnych do siebie elementach)
// sort
def sort[A](list: List[A]): List[A] = {
    def mySortReverse[B](arr: List[B], acc: List[B] = List()): List[B] = arr match {
       case head :: tail => mySortReverse(tail, head :: acc)
       case Nil => acc
    }
    def sortHelp(list: List[A], acc: List[A] = List()): List[A] = {
        def greaterThan[D](el1: D, el2: D): Boolean = {
            el1 match {
                case _: Int => if (el1.toString.toInt > el2.toString.toInt) then true else false
                case _: String => if (el1.toString.toString > el2.toString) then true else false
                case _: Char => if (el1.toString > el2.toString) then true else false
                case _ => throw new Exception("----------- SORT IS SAYING: ELEMENTS NOT COMPARABLE -----------")
            }
        }
        def minimalElement(list: List[A], mini: A): A = {
            list match {
                case head :: tail if greaterThan(mini, head) => minimalElement(tail, head)
                case head :: tail => minimalElement(tail, mini)
                case _ => mini
            }
        }
        def myDelete(list: List[A], el: A, acc: List[A] = List(), isDeleted: Boolean = false): List[A] = {
            list match {
                case head :: tail if (isDeleted) => myDelete(tail, el, head :: acc, true)
                case head :: tail if (head == el) => myDelete(tail, el, acc, true)
                case head :: tail => myDelete(tail, el, head :: acc, false)
                case _ => mySortReverse(acc)
            }
        }
        list match {
            case head :: tail => sortHelp(myDelete(list, minimalElement(list, head)), minimalElement(list, head) :: acc)
            case _ => mySortReverse(acc)
        }
    }
    sortHelp(list)
}

// pack
def pack[A](list: List[A]): List[List[A]] = {
    def packHelp(list: List[A], acc: List[List[A]] = List()): List[List[A]] = {
        acc match {
            case _ if ((list).length == 0) => acc
            case head :: tail if(list.head == acc.head.head) => packHelp(list.tail, (list.head :: acc.head) :: acc.tail)
            case _ => packHelp(list.tail, List(list.head) :: acc)
        }
    }
    packHelp(list).reverse
}

def reduce[A](list: List[A])(f: (A, A) => A): A = {
    val first = last(reverseList(list))
    def reduceHelp(list: List[A], acc: A = first, idx: Int = 0): A = list match {
        case head :: tail if idx != 0 => reduceHelp(tail, f(acc, head), idx + 1)
        case head :: tail => reduceHelp(tail, acc, idx + 1)
        case _ => acc
    }
    reduceHelp(list)
}

// groupBy - po identity
def groupBy[A](ls: List[A]) : Map[A,List[A]] = {
  @annotation.tailrec
  def groupByMyHelper(ls: List[A], acc: Map[A, List[A]] = Map () ): Map[A, List[A]] = ls match {
    case Nil => acc
    case head :: tail => groupByMyHelper(tail, acc.updated (head, head :: acc.getOrElse(head, Nil)))
  }
  groupByMyHelper(ls)
}

@main def mainProg: Unit = {
  val primeNum = 5
  val nonPrimeNum = 6
  val str = "Ala ma kota"
  val listOther = List("Ania", true, 5, 7, -5)
  val listInt = List(5, 1, 2, 5, 7, -5, 12)
  val listInt2 = List(1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 7, 8, 2)
  val listStr = List("Anna", "Kasia", "Irena")
  val listTest = List(2,2)
  println(reduce(listTest)(_ + _))
  println(listTest.reduce(_ + _))
  println("Welcome to code snippets for Scala")

  // --- isPrime ---
  // println(isPrime(primeNum))
  // println(isPrime(nonPrimeNum))

  // --- reverseString ---
  // println(reverseString(str))

  // --- intToBin ---
  // println(intToBin(107))

  // --- last ---
  // println(last(listOther))
  // println(last(listInt))

  // --- compress & deStutter ---
  // println(compress(listInt2))
  // println(deStutter(listInt2))

  // -- reverseList ---
  // println(reverseList(listOther))

  // --- take ---
  // println(take(listOther, 3))

  // --- drop ---
  // println(drop(listOther, 2))

  // --- length ---
  // println(length(listOther))
  // println(length(listInt2))

  // --- map ---
  // println(map(listInt, x => x * 2))
  // println(map(listStr, x => x.length))

  val listlistListInt = List(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

  // -- flatten ---
  // println(listlistListInt.flatten) // List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  // println(flatten(listlistListInt)) // List(1, 2, 3, 4, 5, 6, 7, 8, 9) - TROCHĘ INNE ZACHOWANIE

  // --- flatMap ---
  // println(flatMap(listlistListInt, x => x)) // List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  // println(flatMap2(listlistListInt, x => x)) // List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) - more flatMap-like

  // --- dropKthElem ---
  // println(dropKthElem(listOther, 3))

  // --- filter ---
  // println(filter(listInt, x => x % 2 == 0))

  // --- split ---
  // println(split(str, " "))

  val listInt3 = List(1,4,6,7,8,3,6,8,20,-5)
  // --- max ---
  // println(max(listInt))

  // --- foldLeft ---
  // val resMethod1 = listInt.foldLeft(0)((acc, curr) => acc + curr)
  // println(resMethod1)
  // val resMy1 = foldLeft(listInt)(0)((acc, curr) => acc + curr)
  // println(resMy1)
  // val resMethod2 = listStr.foldLeft("")((acc, curr) => acc + s"$curr ")
  // println(resMethod2)
  // val resMy2 = foldLeft(listStr)("")((acc, curr) => acc + s"$curr ")
  // println(resMy2)

  // --- concat ---
  // println(concat(listInt3, listOther))

  // --- sliding ---
  // val ( list1, len1, shift1 ) = ( List(1,2,3,4,5), 3, 1 )
  // println(sliding(list1)(len1, shift1))
  // val ( list2, len2, shift2 ) = ( List(1,2,3,4,5), 2, 2 )
  // println(sliding(list2)(len2, shift2))
  // val ( list3, len3, shift3 ) = ( List(1,2,3,4,5,6,7,8,9), 4, 2 )
  // println(sliding(list3)(len3, shift3))

  // --- getElementByIndex ---
  // println(getElementByIndex(listOther, 1))

  // --- insertElementInIndex ---
  // println(listOther)
  // println(insertElementInIndex(listOther, false, 2))

  // --- sort ---
  // println(sort(listInt))
  // println(sort(listInt3))

  // --- pack ---
  // println(pack(listInt2))

  // --- groupBy po identity ---
  // println(groupBy(listInt2))
}


// groupBy too advanced
@main def hello: Unit =
  val list = List(1, 1, 1, 2, 2, 3)
  val res1 = list.groupBy2(identity)
  val res2 = groupBy3(list)(identity) // identity - to samo co: l => l
  println(res1)
  println(res2)

extension[A: Ordering] (c: List[A])
  def groupBy2[Key](f: A => Key): Map[Key, List[A]] =
    def helper(cc: List[A], acc: Map[Key, List[A]]): Map[Key, List[A]] = cc match
      case head :: tail => helper(tail, acc + (f(head) -> (head :: acc.getOrElse(f(head), List(head)))))
      case Nil => acc

    helper(c, Map.empty)


def groupBy3[A, Key](c: List[A])(f: A => Key): Map[Key, List[A]] =
  def helper(cc: List[A], acc: Map[Key, List[A]]): Map[Key, List[A]] = cc match
    case head :: tail => helper(tail, acc + (f(head) -> (head :: acc.getOrElse(f(head), List(head)))))
    case Nil => acc

  helper(c, Map.empty)




