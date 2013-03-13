package org.entish

object FibonacciADay {

  /* classic recursion */
  def fibonacci_1(n: Int): Int = n match {
    case 0 ⇒ 0
    case 1 ⇒ 1
    case m ⇒ fibonacci_1(m - 1) + fibonacci_1(m - 2)
  }

  /* classic iteration : no overflow detection good to 46 */

  def fibonacci_2(n: Int): Int = n match {
    case 0 ⇒ 0
    case 1 ⇒ 1
    case m ⇒ {
      var u = 0
      var v = 1
      var t = 1
      for (i ← 1 to m - 1) {
        t = u + v
        u = v
        v = t
      }
      t
    }
  }

  /* classic iteration — Long return type : no overflow detection — good til 164 */

  def fibonacci_3(n: Int): Long = n match {
    case 0 ⇒ 0L
    case 1 ⇒ 1L
    case m ⇒ {
      var u = 0L
      var v = 1L
      var t = 1L
      for (i ← 1 to m - 1) {
        t = u + v
        u = v
        v = t
      }
      t
    }
  }

  /* out of bounds */
  def fibonacci_4(n: Int): Option[Int] = n match {
    case m if m < 0 ⇒ None
    case m          ⇒ Some(fibonacci_1(n))
  }

}
