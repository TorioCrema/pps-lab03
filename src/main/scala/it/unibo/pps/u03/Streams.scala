package u03

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if predicate(h()) => cons(h(), takeWhile(t())(predicate))
      case _ => empty()

    def fill[A](n: Int)(elem: A): Stream[A] = n match
      case 0 => Empty()
      case n => Stream.cons(elem, fill(n - 1)(elem))

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
        case (Cons(h1, t1), s) => cons(h1(), interleave(s, t1()))
        case (Empty(), s) => s

    def cycle[A](s: Stream[A]): Stream[A] =
      def cycleStep(s: Stream[A], original: Stream[A]): Stream[A] = s match
        case Cons(h, t) => cons(h(), cycleStep(t(), original))
        case _ => cycleStep(original, original)
      cycleStep(s, s)


  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  def getFibonacci(n: Int): Int = n match
    case 0 => 0
    case 1 => 1
    case n => getFibonacci(n - 1) + getFibonacci(n - 2)

  val fibonacci: Stream[Int] = Stream.map(Stream.iterate(0)(_ + 1))(getFibonacci)
  println(Stream.toList(Stream.take(fibonacci)(5)))

  val s1 = Stream.take(Stream.iterate(0)(_ + 1))(4)
  val s2 = Stream.take(fibonacci)(3)
  println(Stream.toList(s1))
  println(Stream.toList(s2))
  println(Stream.toList(Stream.interleave(s1, s2)))

  val repeat = Stream.cycle(Stream.take(Stream.iterate(0)(_ + 1))(3))
  println(Stream.toList(Stream.take(repeat)(7)))