package u03

import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @annotation.tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s, n) match
      case (s, 0) => s
      case (Cons(_, t), n) => skip(t)(n - 1)
      case _ => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(hf, tf), Cons(hs, ts)) => Cons((hf, hs), zip(tf, ts))
      case _ => Nil()

    @annotation.tailrec
    def zipTail[A, B](first: Sequence[A], second: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (first, second) match
      case (Cons(hf, tf), Cons(hs, ts)) => zipTail(tf, ts, concat(acc, Cons((hf, hs), Nil())))
      case (_, _) => acc

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Nil(), s2) => s2
      case (Cons(h, t), s2) => Cons(h, concat(t, s2))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
      case _ => Nil()

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
    @annotation.tailrec
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h1, Cons(h2, t)) => min(Cons(if h1 < h2 then h1 else h2, t))
      case Cons(h1, Nil()) => Optional.Just(h1)
      case _ => Optional.Empty()

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
      case _ => Nil()

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @annotation.tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = (s, elem) match
      case (Cons(h, _), e) if h == e => true
      case (Cons(_, t), e) => contains(t)(e)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => Cons(h, filter(s)(_ != h))
      case _ => Nil()

    @annotation.tailrec
    def distinctTail[A](s: Sequence[A], found: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => distinctTail(t, if !contains(found)(h) then concat(found, Cons(h, Nil())) else found)
      case _ => found

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Cons(h, t) => Cons(Cons(h, filterUntil(t)(_ != h)), group(filterAsLongAs(t)(_ == h)))
      case _ => Nil()

    def filterUntil[A](s: Sequence[A])(pred: A => Boolean): Sequence[A] = s match
      case Cons(h, t) if !pred(h) => Cons(h, filterUntil(t)(pred))
      case _ => Nil()

    def filterAsLongAs[A](s: Sequence[A])(pred: A => Boolean): Sequence[A] = s match
      case Cons(h, t) if pred(h) => filterAsLongAs(t)(pred)
      case _ => s

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
      case s => (filter(s)(pred), filter(s)(!pred(_)))

    @annotation.tailrec
    def foldLeft[A, B](s: Sequence[A])(default: B)(acc: (B, A) => B): B = s match
      case Cons(h, t) => foldLeft(t)(acc(default, h))(acc)
      case _ => default

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
