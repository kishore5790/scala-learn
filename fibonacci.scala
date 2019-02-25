def fibonacci(n: Int): Int = {
  @annotation.tailrec
  def go(prev_val: Int, curr_val: Int, iter: Int, maxiter: Int): Int =
    if (iter == maxiter) curr_val
    else go(curr_val, curr_val + prev_val, iter+1, maxiter)
    go(0, 1, 2, n)
}
