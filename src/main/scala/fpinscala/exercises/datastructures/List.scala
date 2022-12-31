package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil         => sys.error("tail on empty List")
      case Cons(_, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil         => sys.error("setHead on empty List")
      case Cons(_, xs) => Cons(h, xs)

  def drop[A](l: List[A], n: Int): List[A] =
    @annotation.tailrec
    def loop(l: List[A], n: Int): List[A] =
      if n <= 0 then l
      else
        l match
          case Nil           => List()
          case Cons(_, tail) => loop(tail, n - 1)
    loop(l, n)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    def loop(l: List[A]): List[A] =
      l match
        case Nil => l
        case Cons(head, tail) =>
          if f(head) then loop(drop(l, 1))
          else l
    loop(l)

  // author's solution, beautiful
  // def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  //   l match
  //     case Cons(head, tail) if f(head) => dropWhile(tail, f)
  //     case _                           => l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil              => sys.error("List(Nil)")
      case Cons(head, Nil)  => Nil
      case Cons(head, tail) => Cons(head, init(tail))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case List.Nil        => acc
      case List.Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    // the author uses (acc, h) to describe x, y. Those are the correct names
    foldLeft(l, List[A](), (x, y) => List.Cons(y, x))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    // List.Cons(_, _)
    foldRight(l, r, (acc, h) => List.Cons(acc, h))

  def concat[A](l: List[List[A]]): List[A] =
    /// foldRight(reverse(l), List[A](), (acc, h) => appendViaFoldRight(h, acc))
    foldLeft(l, List[A](), appendViaFoldRight)

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List[Int](), (h, acc) => List.Cons(h + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (h, acc) => List.Cons(h.toString, acc))

  def map[A, B](l: List[A], f: A => B): List[B] =
    // `foldRight` is not stack-safe. We can
    // use `foldRightViaFoldLeft` to avoid the stack overflow
    foldRight(l, List[B](), (h, acc) => List.Cons(f(h), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldLeft(
      reverse(as),
      List[A](),
      (acc, h) => if f(h) then List.Cons(h, acc) else acc
    )

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, h => if f(h) then List.Cons(h, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    // I was tired so I just saw the solution, truly beautiful
    (a, b) match
      case (Nil, b)                     => Nil
      case (a, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Nil, b)                     => Nil
      case (a, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def go[A](l: List[A], sub: List[A]): Boolean =
      (l, sub) match
        case (Cons(h1, t1), Cons(h2, t2)) if l == sub => true
        case (_, Nil)                                 => true
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(drop(l, 1), sub) || go(init(l), sub)
        case (Nil, _) => false

    go(sup, sub)
