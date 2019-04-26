// Exercise 4.1: Implement all of the functions on Option
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  } 
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  } 

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  override def toString(): String = this match {
    case None => "None"
    case Some(a) => "Some(" + a + ")"
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// Exercise 4.2: Implement the variance function in terms of flatMap
def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

// For the variance function, the formula var(X) = mean(X^2)-(mean(X)^2) is used.
// This formula is not typically used in real applications (catastrophic cancellation).
def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs.map(x => math.pow(x, 2))).flatMap(x => Some(x - math.pow(mean(xs).getOrElse(0), 2)))
}

def half(x: Int): Option[Int] = if (x%2 == 1) None else Some(x/2)

// A function that lifts another function A => B to Option[A] => Option[B]
def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

// Re-write the half function so that it returns an Int first
def half0(x: Int): Int = x / 2

// Lift half0
val ohalf = lift(half0)
println(ohalf(Some(12)))
println(ohalf(Some(15)))

// Tests
val x = Some(15)
println(x)
println(x.map(x => x * x))
println(x.flatMap(x => half(x)))
println(x.map(x => x * x).flatMap(x => half(x)))
println(x.map(x => x * 2).flatMap(x => half(x)))
println(x.map(x => x * x).flatMap(x => half(x)).getOrElse(-1))
println(x.map(x => x * x).flatMap(x => half(x)).orElse(Some(100)))
println(x.map(x => x * x).flatMap(x => half(x)).orElse(Some(100)).filter(x => x % 12 == 0).getOrElse(0))

val array = Array(3.0, 3.5, 4.0, 4.1)
println(variance(array))

// Convert a exception throwing function to an Option returning function, swallowing the exception
val str = "Hello, World!"
//println(str.charAt(13)) --- this throws an StringIndexOutOfBoundsException exception

// Now let us wrap charAt into a function and return an Option instead
def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

println(Try { str.charAt(4) })
println(Try { str.charAt(13) })
// Wow, lifting throwing APIs to Option-oriented APIs is so easy!
