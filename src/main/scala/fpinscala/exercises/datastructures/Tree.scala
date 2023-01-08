package fpinscala.exercises.datastructures
import scala.math.*

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int =
    // I saw the solution by mistake so I'll just paste it.
    this match
      case Leaf(_)             => 0
      case Branch(left, right) => 1 + (left.depth.max(right.depth))

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int =
    fold((leaf) => 1, (left, right) => 1 + left + right)
    // cleaner solution from the author
    // fold(a => 1, 1 + _ + _)

  def depthViaFold: Int =
    fold(leaf => 0, (left, right) => 1 + left.max(right))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(leaf => Leaf(f(leaf)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int])
    def firstPositive: Int = t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int])
    def maximum: Int = t match
      case Leaf(i)      => i
      case Branch(l, r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
