package tasks

import org.junit.Assert.assertEquals
import org.junit.Test
import tasks.Lab3.*
import u02.Modules.Person.*
import u03.Sequences.Sequence.*
import u03.Streams.*
import u03.Streams.Stream.*

class Lab3Test:

  // -------------------------------------------------- TASK 2 --------------------------------------------------

  val sequence = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"),
      Cons(Student("Terenzi", 2025), Cons(Teacher("Viroli", "OOP"), Nil()))))

  @Test def testGetCourses() =
    assertEquals(Cons("PPS", Cons("PCD", Cons("OOP", Nil()))), getCourses(sequence))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testGetNumberCourses() =
    assertEquals(3, getNumberCourses(sequence))

  // -------------------------------------------------- TASK 3 --------------------------------------------------

  @Test def testFill() =
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))

  @Test def testFibonacci() =
    val fib5 = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    val fibonacci: Stream[Int] = Stream.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), Stream.toList(Stream.take(fibonacci)(5)))