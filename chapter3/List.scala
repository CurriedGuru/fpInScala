sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*):  List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]) = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](list: List[A], y: A) = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(y, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (n == 0) return Cons(x, xs) else drop(xs, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f) //else Cons(x, xs)
    case _ => l
  }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = 
    l match {
      case Cons(x, xs) if f(x) => dropWhileCurried(xs)(f)
      case _ => l 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) =>  f(x, foldRight(xs, z)(f))
  }

  def sum0(as: List[Int]): Int = {
    foldRight(as, 0)((x, y) => x + y)
  }

  def product0(as: List[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  //Exercise 3.9: Compute the length of a list using foldRight
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)  //y is the accumulator variable
  }

  //Exercise 3.10: Write a tail-recursive version of foldRight, called foldLeft
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec // This annotation will help us assert that the said function is indeed tail recursive!
    def accumulate[A, B](as: List[A], accumulator: B)(f: (B, A) => B): B = as match {
      case Nil => accumulator
      case Cons(x, xs) => accumulate(xs, f(accumulator, x))(f)
    }

    accumulate(as, z)(f)
  }

  // Exercise 3.11
  def sum1(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  // Exercise 3.11
  def product1(as: List[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }

  // Exercise 3.11
  def length1[A](as: List[A]): Int = {
    foldLeft(as, 1)((x, _) => x + 1)
  }

  // Exercise 3.13: Implementing foldLeft in terms of foldRight
  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((x, y) => f(y, x))

  // Exercise 3.13: Implementing foldRight in terms of foldLeft
  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((x, y) => f(y, x))

  // Exercise 3.14: Implement append in terms of foldRight
  def append0[A](a1: List[A], a2: List[A]) = {
      foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  def appendToEnd[A](a: List[A], a0: A): List[A] = a match {
      case Nil => List.apply(a0)
      case Cons(x, xs) => Cons(x, appendToEnd(xs, a0))
  }
      
  // Exercie 3.14: Implement append in terms of foldLeft
  def append1[A](a1: List[A], a2: List[A]) = {
      foldLeft(a2, a1)((x, y) => appendToEnd(x, y))
  }

  // Exercise 3.15: Write a function to concatenate a list of lists into a list
  def concatenate[A](lists: List[List[A]]): List[A] = lists match {
      case Nil => Nil
      case Cons(list, Nil) => list
      case Cons(list, lists) => append(list, concatenate(lists))
  }

  // Exercise 3.18: Write a function map that generalizes modifying each element of a list while maintaining the structure of the list
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Exercise 3.16: Write a function that transforms a list of integers by adding 1 to each element
  def addOne(ints: List[Int]): List[Int] = List.map(ints)(x => x + 1)

  // Exercise 3.17: Write a function that turns each value in a List[Double] into a String
  def toString(doubles: List[Double]): List[String] = List.map(doubles)(d => d.toString)

  // Exercise 3.19: Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd
  // numbers from a List[Int]
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def removeOddNumbers(ints: List[Int]): List[Int] = List.filter(ints)(int => int % 2 == 0)

  // Exercise 3.20: Write a function flatMap that returns a list instead of a single result, and that list should be inserted into the final list.
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => List.append0(f(x), flatMap(xs)(f))
  }

  // Exercise 3.21: Use flatMap to implement filter
  def filter0[A](as: List[A])(f: A => Boolean) = flatMap(as)(x => if (f(x)) List.apply(x) else List.apply())

  // Exerise 3.23: Implement zipWith for lists of arbitrary lengths
  def zipWith[A](a1: List[A], a2: List[A], padding: A)(f: (A, A) => A): List[A] = a1 match {
    case Nil => a2 match {
        case Nil => Nil
        case Cons(a2, a2s) => Cons(f(padding, a2), zipWith(Nil, a2s, padding)(f))
    }
    case Cons(a1, a1s) => a2 match {
        case Nil => Cons(f(a1, padding), zipWith(a1s, Nil, padding)(f))
        case Cons(a2, a2s) => Cons(f(a1, a2), zipWith(a1s, a2s, padding)(f))
    }
  }
}
