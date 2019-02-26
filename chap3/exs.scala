object mod {
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Cons(x, y) => y
      case Nil => Nil
    }
  }

  def setHead[A](as: List[A], rep: A): List[A] = {
    as match {
      case Cons(x, y) => Cons(rep, y)
      case Nil => Nil
    }
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def tail_one(as: List[A], current: Int, max: Int): List[A] = {
      if(current < max) {
        val as2 = as match {
          case Cons(x, y) => y
          case Nil => Nil
        }
        tail_one(as2, current + 1, max)
      }
      else {
        as
      }
    }
    tail_one(as, 0, n)
  }

  //mod.dropWhile(List(2,2,2,3,2,1), (x: Int) => x==2)
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def tail_one(as: List[A], f: A => Boolean): List[A] = {
      as match {
        case Cons(x, y) => if(f(x)) tail_one(y, f) else as
        case Nil => as
      }
    }
    tail_one(as, f)
  }

  //mod.init(List(2,2,2,3,2,1))
  def init[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def loop(left: List[A], right: List[A]): List[A] = {
      left match {
        case Cons(x, Cons(y, z)) => loop(Cons(y, z), Cons(x, right))
        case Cons(x, Nil) => right
        case _ => right
      }
    }
    @annotation.tailrec
    def rev(left: List[A], right: List[A]): List[A] = {
      left match {
        case Cons(x, y) => rev(y, Cons(x, right))
        case Nil => right
      }
    }
    val as2 = loop(as, Nil)
    rev(as2, Nil)
  }
}

