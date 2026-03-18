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
    def zipTail[A, B](first: Sequence[A], second: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (first, second, acc) match
      case (Nil(), _, acc) => acc
      case (_, Nil(), acc) => acc
      case (Cons(hf, tf), Cons(hs, ts), acc) => zipTail(tf, ts, concat(acc, Cons((hf, hs), Nil())))

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
      case Nil() => Nil()
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = (s, mapper) match
      case (Cons(h, t), m) => concat(m(h), flatMap(t)(m))
      case _ => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h, t) if Optional.orElse(min(t), h) >= h => Optional.Just(h)
      case Cons(h, t) => min(t)
      case _ => Optional.Empty()

    @annotation.tailrec
    def minTail(s: Sequence[Int], min: Optional[Int]): Optional[Int] = (s, min) match
      case (Cons(h, Nil()), Optional.Just(n)) if h < n => Optional.Just(h)
      case (Cons(h, Nil()), Optional.Just(n)) if n <= h => Optional.Just(n)
      case (Cons(h, t), Optional.Just(n)) if h < n => minTail(t, Optional.Just(h))
      case (Cons(h, t), Optional.Just(n)) if n <= h => minTail(t, Optional.Just(n))
      case (Cons(h, t), Optional.Empty()) => minTail(t, Optional.Just(h))
      case (_, n) => n

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
    def distinct[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def distinctTail(s: Sequence[A], found: Sequence[A]): Sequence[A] = (s, found) match
        case (Cons(h, t), Nil()) => distinctTail(t, Cons(h, Nil()))
        case (Cons(h, t), f) if !contains(f)(h) => distinctTail(t, concat(f, Cons(h, Nil())))
        case (Cons(h, t), f) => distinctTail(t, f)
        case (_, f) => f

      distinctTail(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @annotation.tailrec
      def groupBy(s: Sequence[A], groups: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] =
        (s, groups, acc) match
          case (Cons(h, t), Cons(gH, gT), Cons(aH, aT)) if h == gH => groupBy(t, Cons(gH, gT), Cons(Cons(h, aH), aT))
          case (Cons(h, t), Cons(gH, gT), Cons(aH, aT)) => groupBy(t, gT, Cons(Cons(h, Nil()), Cons(aH, aT)))
          case (Cons(h, t), Cons(gH, gT), Nil()) if h == gH => groupBy(t, Cons(gH, gT), Cons(Cons(h, Nil()), Nil()))
          case (Cons(h, t), Cons(gH, gT), Nil()) => groupBy(t, gT, Nil())
          case (_, _, acc) => acc

      reverse(groupBy(s, distinct(s), Nil()))

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
