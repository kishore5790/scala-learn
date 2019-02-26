// output each element of the original list num times
def f(num:Int, arr:List[Int]): List[Int] = {
    @annotation.tailrec
    def g(num:Int, x:Int, l: List[Int]): List[Int] = {
        if(num == 0) l
        else g(num-1, x, x::l)
    }
    arr.flatMap((x: Int) => g(num, x, List()))
}

