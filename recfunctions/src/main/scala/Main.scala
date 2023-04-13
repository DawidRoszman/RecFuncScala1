@main def mainProg: Unit = {
  println("Hello, world!")
}

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

def reverseString(napis: String): String = {
    //@annotation.tailrec
    def go(lista: List[String], aku: List[String]): String = {
        lista match {
            case Nil => aku.mkString("")
            case head :: next => go(next, head :: aku)
            case null => "twoj stary"
        }
    }
    go(napis.split("").toList,Nil) //split() zwraca Array a nie List
}

def IntToBin(liczba: Int):Int = {
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

def last(list: List[Int]): Int ={
    def go(list: List[Int]): Int = {
        list match {
            case head::Nil => head
            case head :: tail => go(tail)
            case Nil => -2137
        }
    }
    go(list)
}

def compress(list: List[Int]): List[Int] = {
    def go(list: List[Int], aku: List[Int], lastElement: Int): List[Int]={
        list match{
            case Nil => reverseList(aku)
            case head :: next =>  if lastElement == head then go(next, aku, lastElement) else go(next, head :: aku, head)
        }
    }
    go(list, Nil, -2137)
}

def reverseList[A](list: List[A]): List[A] = {
  def go[A](list: List[A], acc: List[A]): List[A]={
      list match {
        case Nil => acc
        case head :: next => go(next, head :: acc)
      }
  }
  go(list, Nil)
}

def take[A](list: List[A], n: Int): List[A] ={
  def go[A](list: List[A], n: Int, acc : List[A]): List[A] = {
      (n, list) match
        case (0, _) => acc
        case (_, head::tail) => go(tail, n-1, head::acc)
        case (_, _) => acc


  }
  reverseList(go(list, n, Nil))
}


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
def map[A](list: List[A], fun: A => A): List[A] = {
  def go(list: List[A], aku: List[A]): List[A]= {
      list match{
          case Nil => reverseList(aku)
          case head::tail => go(tail,fun(head)::aku)
      }
  }
  go(list,Nil)
}

//flatten
def flatten(list: List[Any]): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match{
        case Nil => reverseList(aku)
        case (head : List[Any]) :: tail => go(head ::: tail, aku)
        case head :: tail => go(tail, head:: aku )
      }
  }
  go(list,Nil)
}

// flatmap
def flatMap(list: List[Any], fun: Any => Any): List[Any] = {
  def go(list: List[Any], aku: List[Any]): List[Any] = {
      list match {
          case Nil => reverseList(aku)
          case head::tail => go(tail,fun(head)::aku)
      }
  }
  flatten(go(list,Nil))
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

