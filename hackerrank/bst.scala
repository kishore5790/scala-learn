// check if valid bst
object Solution {
  def is_valid_bst(as: List[Int]): Boolean = {
    if (as == Nil) return(true)
    val head = as.head
    val bs = as.tail
    val lefts = bs.takeWhile((n: Int) => n < head)
    val rights = bs.dropWhile((n: Int) => n < head)
    val check_rights = rights.dropWhile((n: Int) => n > head)
    is_valid_bst(lefts) && is_valid_bst(rights) && check_rights == Nil
  }

  def main(args: Array[String]) {
    // throw away everything other than what we need
    @annotation.tailrec
    def take_alternates(as: List[String], bs: List[String], index: Int): List[String] = {
      if(as == Nil)
        bs
      else if (index % 2 == 1)
        take_alternates(as.tail, as.head :: bs, index + 1)
      else
        take_alternates(as.tail, bs, index + 1)
    }
    val input = io.Source.stdin.getLines().toList.tail
    val lines = take_alternates(input, Nil, 0).reverse
    for (line <- lines) {
      val ints = line.split(" ").toList.map(_.toInt)
      val is_valid = is_valid_bst(ints)
      if(is_valid)
        println("YES")
      else
        println("NO")
    }
  }
}

