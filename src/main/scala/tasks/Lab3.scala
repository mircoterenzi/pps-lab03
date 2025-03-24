package tasks

import u02.Modules.Person
import u02.Modules.Person.*
import u03.Optionals.Optional
import u03.Optionals.Optional.{Empty, Just, orElse}
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, filter, map}
import u03.Streams.*
import u03.Streams.Stream.*

import scala.annotation.tailrec

object Lab3:

  // -------------------------------------------------- TASK 1 --------------------------------------------------

  /*
       * Skip the first n elements of the sequence
       * E.g., [10, 20, 30], 2 => [30]
       * E.g., [10, 20, 30], 3 => []
       * E.g., [10, 20, 30], 0 => [10, 20, 30]
       * E.g., [], 2 => []
       */
  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  /*
   * Zip two sequences
   * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
   * E.g., [10], [] => []
   * E.g., [], [] => []
   */
  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  /*
   * Concatenate two sequences
   * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
   * E.g., [10], [] => [10]
   * E.g., [], [] => []
   */
  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case _ => s2

  /*
   * Reverse the sequence
   * E.g., [10, 20, 30] => [30, 20, 10]
   * E.g., [10] => [10]
   * E.g., [] => []
   */
  def reverse[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def reverseTail(s: Sequence[A], acc: Sequence[A]) : Sequence[A] = s match
      case Cons(h, t) => reverseTail(t, Cons(h, acc))
      case _ => acc
    reverseTail(s, Nil())

  /*
   * Map the elements of the sequence to a new sequence and flatten the result
   * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
   * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
   * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
   */
  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  /*
   * Get the minimum element in the sequence
   * E.g., [30, 20, 10] => 10
   * E.g., [10, 1, 30] => 1
   */
  def min(s: Sequence[Int]): Optional[Int] = s match
    case Cons(h, t) if h <= orElse(min(t), h) => Just(h)
    case _ => Empty()

  /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t1) => t1 match
      case Cons(_, t2) => Cons(h, evenIndices(t2))
      case _ => Cons(h, evenIndices(Nil()))
    case _ => Nil()

  /*
   * Check if the sequence contains the element
   * E.g., [10, 20, 30] => true if elem is 20
   * E.g., [10, 20, 30] => false if elem is 40
   */
  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) => h == elem || contains(t)(elem)
    case _ => false

  /*
   * Remove duplicates from the sequence
   * E.g., [10, 20, 10, 30] => [10, 20, 30]
   * E.g., [10, 20, 30] => [10, 20, 30]
   */
  def distinct[A](s: Sequence[A]): Sequence[A] =
    def _distinct[B](s: Sequence[B], acc: Sequence[B]): Sequence[B] = s match
      case Cons(h, t) if contains(acc)(h) => _distinct(t, acc)
      case Cons(h, t) => Cons(h, _distinct(t, Cons(h, acc)))
      case _ => Nil()
    _distinct(s, Nil())

  /*
   * Group contiguous elements in the sequence
   * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
   * E.g., [10, 20, 30] => [[10], [20], [30]]
   * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
   */
  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    def _group[B](s: Sequence[B], group: Sequence[B]): Sequence[Sequence[B]] = s match
      case Cons(h1, t1) => t1 match
        case Cons(h2, t2) if h1 == h2 => _group(Cons(h2, t2), Cons(h1, group))
        case Cons(h2, t2) => Cons(Cons(h1, group), _group(Cons(h2, t2), Nil()))
        case _ => Cons(Cons(h1, group), Nil())
      case _ => Nil()
    _group(s, Nil())

  /*
   * Partition the sequence into two sequences based on the predicate
   * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
   * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
   */
  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    (filter(s)(pred), filter(s)(!pred(_)))

  // -------------------------------------------------- TASK 2 --------------------------------------------------

  def getCourses(s: Sequence[Person]): Sequence[String] = flatMap(s) {
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil()
  }

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(acc: B)(op: (B, A) => B): B = s match
    case Cons(h,t) => foldLeft(t)(op(acc, h))(op)
    case _ => acc
    
  def getNumberCourses(s: Sequence[Person]): Int =
    val teachers = filter(s) {
      case Teacher(_, _) => true
      case _ => false
    }
    foldLeft(map(teachers) { case Teacher(_, c) => c })(0)((acc, elem) => acc + 1)

  // -------------------------------------------------- TASK 3 --------------------------------------------------
  
  def fill[A](n: Int)(k: A): Stream[A] = n match
    case 0 => empty()
    case _ => cons(k, fill(n - 1)(k))