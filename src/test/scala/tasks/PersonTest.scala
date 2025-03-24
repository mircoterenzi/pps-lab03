package tasks

import org.junit.Assert.*
import org.junit.Test
import tasks.Lab3.*
import u02.Modules.Person.*
import u03.Sequences.Sequence.*

class PersonTest:

  val sequence = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"),
      Cons(Student("Terenzi", 2025), Cons(Teacher("Viroli", "OOP"), Nil()))))

  @Test def testGetCoursesBy() =
    assertEquals(Cons("PPS", Cons("PCD", Cons("OOP", Nil()))), getCourses(sequence))
