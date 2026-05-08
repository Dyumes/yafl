package yafl

/** Returns the start of the partition in `self` whose elements satisfy `isOnRHS`.
  *
  * `self` must already be partitioned according `isOnRHS`. That is, there exists `i` such that
  * `self.drop(i).forall(isOnRHS)` and `isOnRHS(self(j))`for any positive `j` less than `i`.
  *
  *  - Complexity: O(log N) where N is the lenght of `this`.
  */
extension [T](self: IndexedSeq[T]) def partitioningIndexWhere(isOnRHS: T => Boolean): Int =
  def find(l: Int, n: Int): Int =
    if n == 0 then l else
      val h = n / 2
      val m = l + h
      if isOnRHS(self(m)) then find(l, h) else find(m + 1, n - (h + 1))
  find(0, self.length)
