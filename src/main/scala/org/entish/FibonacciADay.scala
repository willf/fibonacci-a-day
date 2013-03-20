package org.entish
import scala.annotation.switch
import scala.annotation.tailrec

object FibonacciADay {

  /* classic recursion -- not terrible until about F(40); accurate to 46 */
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

  /* Double — accurate to F(76) -- that is, math.round(fibonacci_4(76)) == fibonacci_7(76) */
  def fibonacci_4(n: Int): Double = n match {
    case 0 ⇒ 0.0
    case 1 ⇒ 1.0
    case m ⇒ {
      var u = 0.0
      var v = 1.0
      var t = 1.0
      for (i ← 1 to m - 1) {
        t = u + v
        u = v
        v = t
      }
      t
    }
  }

  /* BigInt */
  def fibonacci_5(n: Int): BigInt = n match {
    case m if m < 0 ⇒ BigInt(-1)
    case 0          ⇒ BigInt(0)
    case 1          ⇒ BigInt(1)
    case m ⇒ {
      var u = BigInt(0)
      var v = BigInt(1)
      var t = BigInt(1)
      for (i ← 1 to m - 1) {
        t = u + v
        u = v
        v = t
      }
      t
    }
  }

  /* tail recursive with accumulator */
  def fibonacci_6(n: Int): BigInt = {
    def recurse(i: Int, u: BigInt, v: BigInt): BigInt =
      if (i == n) v
      else recurse(i + 1, v, u + v)
    n match {
      case m if m < 0 ⇒ BigInt(-1)
      case 0          ⇒ BigInt(0)
      case 1          ⇒ BigInt(1)
      case m          ⇒ recurse(1, 0, 1)
    }
  }

  def fibonacci_6b(n: Int): BigInt = {
    @tailrec
    def recurse(i: Int, u: BigInt, v: BigInt): BigInt =
      if (i == n) v
      else recurse(i + 1, v, u + v)
    n match {
      case m if m < 0 ⇒ BigInt(-1)
      case 0          ⇒ BigInt(0)
      case 1          ⇒ BigInt(1)
      case m          ⇒ recurse(1, 0, 1)
    }
  }

  def fibonacci_6c(n: Int): BigInt = {
    def recurse(i: Int, u: BigInt, v: BigInt): BigInt =
      if (i == n) v
      else recurse(i + 1, v, u + v)
    (n: @switch) match {
      case 0 ⇒ BigInt(0)
      case 1 ⇒ BigInt(1)
      case _ ⇒ if (n < 0) BigInt(-1) else recurse(1, 0, 1)
    }
  }

  def fibonacci_6d(n: Int): BigInt = {
    @tailrec
    def recurse(i: Int, u: BigInt, v: BigInt): BigInt =
      if (i == n) v
      else recurse(i + 1, v, u + v)
    (n: @switch) match {
      case 0 ⇒ BigInt(0)
      case 1 ⇒ BigInt(1)
      case _ ⇒ if (n < 0) BigInt(-1) else recurse(1, 0, 1)
    }
  }

  /* using Phi — good to  70 F(70) = 190392490709135 */
  def fibonacci_7(n: Int) = {
    val sqrt5 = math.sqrt(5.0)
    val φ = (sqrt5 + 1) / 2.0
    math.round(math.pow(φ, n) / sqrt5)
  }

  /* memoization */
  val memo = scala.collection.mutable.HashMap[Int, BigInt]()
  def fibonacci_8(n: Int): BigInt = {
    def fib(i: Int): BigInt =
      memo.getOrElseUpdate(i, fibonacci_8(i))
    n match {
      case 0 ⇒ BigInt(0)
      case 1 ⇒ BigInt(1)
      case m ⇒ fib(m - 1) + fib(m - 2)
    }
  }

  /* final version, and extended to negative numbers */
  def fibonacci(n: Int): BigInt = {
    def fib(m: Int): BigInt = {
      @tailrec def recurse(i: Int, u: BigInt, v: BigInt, limit: Int): BigInt =
        if (i == limit) v
        else recurse(i + 1, v, u + v, limit)
      (m: @switch) match {
        case 0 ⇒ BigInt(0)
        case 1 ⇒ BigInt(1)
        case _ ⇒ recurse(1, 0, 1, m)
      }
    }
    n match {
      case _ if (n >= 0)     ⇒ fib(n) // natural numbers 
      case _ if (n % 2 == 0) ⇒ -fib(-n) // even, negative numbers*/
      case _                 ⇒ fib(-n) // odd, negative numbers
    }
  }

  def benchmark(times: Int, fn: Function0[Any]) = {
    import scala.compat.Platform
    Platform.collectGarbage
    val start = System.nanoTime
    for (i ← 1 to times) {
      fn()
      // Platform.collectGarbage
    }
    ((System.nanoTime - start) / times.toDouble) / 1000000
  }

  /* memoization and the state monad */
  /* http://blog.tmorris.net/posts/memoisation-with-state-using-scala/ */

}

case class State[S, A](run: S ⇒ (A, S)) {
  // 1. the map method
  def map[B](f: A ⇒ B): State[S, B] =
    State(s ⇒ {
      val (a, t) = run(s)
      (f(a), t)
    })

  // 2. the flatMap method
  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State(s ⇒ {
      val (a, t) = run(s)
      f(a) run t
    })

  // Convenience function to drop the resulting state value
  def eval(s: S): A =
    run(s)._1
}

object State {
  // 3. The insert function
  def insert[S, A](a: A): State[S, A] =
    State(s ⇒ (a, s))

  // Convenience function for taking the current state to a value
  def get[S, A](f: S ⇒ A): State[S, A] =
    State(s ⇒ (f(s), s))

  // Convenience function for modifying the current state
  def mod[S](f: S ⇒ S): State[S, Unit] =
    State(s ⇒ ((), f(s)))
}

object FibMemo4 {
  type Memo = Map[BigInt, BigInt]

  def fibmemo4(n: BigInt): BigInt = {
    def fibmemoR(z: BigInt): State[Memo, BigInt] =
      if (z <= 1)
        State.insert(z)
      else
        for {
          u ← State.get((m: Memo) ⇒ m get z)
          v ← u map State.insert[Memo, BigInt] getOrElse (for {
            r ← fibmemoR(z - 1)
            s ← fibmemoR(z - 2)
            t = r + s
            _ ← State.mod((m: Memo) ⇒ m + ((z, t)))
          } yield t)
        } yield v

    fibmemoR(n) eval Map()
  }
}

