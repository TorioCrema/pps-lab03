package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.{Empty, Just}
import u03.Sequences.*
import Sequence.*
import it.unibo.pps.u03.Person.*

class PersonTest:

  val teachers: Sequence[Person] = Cons(Teacher("Alessio", Cons("PPS", Nil())), Cons(Teacher("Alessandro", Cons("PCD", Nil())), Cons(Teacher("Gabriele", Cons("Cybersec", Nil())), Nil())))

  @Test def testIsStudent(): Unit =
    val student: Person = Person.Student("Luca", 2026)
    val teacher: Person = Person.Teacher("Alessio", Cons("PPS", Nil()))
    assertTrue(isStudent(student))
    assertFalse(isStudent(teacher))

  @Test def testGetCourses(): Unit =
    assertEquals(Cons("PPS", Cons("PCD", Cons("Cybersec", Nil()))), getCourses(teachers))
    val teachers2: Sequence[Person] = concat(teachers, Cons(Teacher("Vittorio", Cons("PPS", Nil())), Nil()))
    assertEquals(Cons("PPS", Cons("PCD", Cons("Cybersec", Nil()))), getCourses(teachers2))

  @Test def testCountCourses(): Unit =
    assertEquals(3, countCourses(teachers))