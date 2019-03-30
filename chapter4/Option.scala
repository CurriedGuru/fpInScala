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

def half(x: Int): Option[Int] = if (x%2 == 1) None else Some(x/2)

val x = Some(15)
println(x)
println(x.map(x => x * x))
println(x.flatMap(x => half(x)))
println(x.map(x => x * x).flatMap(x => half(x)))
println(x.map(x => x * 2).flatMap(x => half(x)))
println(x.map(x => x * x).flatMap(x => half(x)).getOrElse(-1))
println(x.map(x => x * x).flatMap(x => half(x)).orElse(Some(100)))
println(x.map(x => x * x).flatMap(x => half(x)).orElse(Some(100)).filter(x => x % 12 == 0).getOrElse(0))
