object mod {
  // remove one element from head
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Cons(x, y) => y
      case Nil => Nil
    }
  }

  // set an element at head
  def setHead[A](as: List[A], rep: A): List[A] = {
    as match {
      case Cons(x, y) => Cons(rep, y)
      case Nil => Nil
    }
  }

  // drop n elements from head
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

  // drop elements from head while they satisfy a condition
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

  // drop one element from the tail
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

