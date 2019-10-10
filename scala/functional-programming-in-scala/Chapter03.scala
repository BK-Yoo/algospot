import scala.annotation.tailrec

object Chapter03 {

  import List._

  def main(args: Array[String]): Unit = {
    val arr123 = List(1, 2, 3)
    val arr321 = List(3, 2, 1)
    val arr12345 = List(1, 2, 3, 4, 5)

    println(tail(arr123) == List(2, 3))
    println(setHead(arr123, 2) == List(2, 2, 3))
    println(drop(arr123, 2) == List(3))
    println(dropWhile(arr12345, (a: Int) => a < 4) == List(4, 5))
    println(init(arr12345) == List(1, 2, 3, 4))
    println(length(arr12345) == 5)
    println(foldLeft(arr123, Nil: List[Int])((z, a) => Cons(a, z)) == arr321)
    println(reverse(List("a", "b", "c")) == List("c", "b", "a"))
    println(
      foldLeftByRight(arr123, Nil: List[Int])((z, a) => Cons(a, z)) == arr321
    )
    println(foldRightByLeft(arr123, Nil: List[Int])(Cons(_, _)) == arr123)
    println(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    println(
      flatten(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6)
    )
  }

}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => Cons(newHead, t)
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _                   => l
  }

  // 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil                      => Nil
    case Cons(_, t) if (t == Nil) => Nil
    case Cons(h, t)               => Cons(h, init(t))
  }

  // 3.7
  def product(ds: List[Double]): Double = foldRight(ds, 0.0)(_ * _)

  // 3.8
  // f(1, f(2, f(3, z))) => f = Cons => List(1,2,3)

  // 3.9
  def length[A](as: List[A]): Int = as match {
    case Nil        => 0
    case Cons(_, t) => 1 + length(t)
  }

  // 3.10
//  foldLeft(arr123, Nil: List[Int])((z, a) => Cons(a, z)) == arr321
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, a) => Cons(a, z))

  // 3.13.1
  def foldLeftByRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // 3.13.2
  def foldRightByLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // 3.14
  def append[A](l: List[A], r: List[A]): List[A] = l match {
    case Nil        => r
    case Cons(h, t) => Cons(h, append(t, r))
  }

  // 3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((a, z) => append(a, z))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
