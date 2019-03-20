sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25: Write a function size that counts the number of nodes in a tree
  def numberOfNodes[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => numberOfNodes(left) + numberOfNodes(right)
  }

  // 3.28: Write a function map that modifies each element of a tree with a given function 
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.26: Write a function depth that returns the maximum path length from the root of a tree to any root
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0 
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // 3.27: Write a function maximum that returns a maximum element in Tree[Int]
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  // 3.29: Generalise size, maximum, depth and map writing a new function fold that abstracts over the similarities.
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def numberOfNodes0[A](tree: Tree[A]): Int = fold(tree)(x => 1)((x, y) => x + y)
  def depth0[A](tree: Tree[A]): Int = fold(tree)(x => 0)((x, y) => 1 + x.max(y)) 
  def maximum0(tree: Tree[Int]): Int = fold(tree)(x => x)((x, y) => x.max(y))
}
