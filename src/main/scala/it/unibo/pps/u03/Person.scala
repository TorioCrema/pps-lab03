package it.unibo.pps.u03
import u03.Sequences.*
import Sequence.*

// An ADT: type + module
enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: Sequence[String])

object Person:
  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  def getCourses(people: Sequence[Person]): Sequence[String] =
    distinct(flatMap(filter(people)(!isStudent(_)))({case Teacher(_, c) => c}))

  def countCourses(people: Sequence[Person]): Int =
    sum(map(filter(people)(!isStudent(_)))({case Teacher(_, c) => foldLeft(c)(0)((x, y) => x + 1)}))
