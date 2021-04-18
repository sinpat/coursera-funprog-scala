package funsets

import org.junit._

/** This class is a test suite for the methods in object FunSets.
  *
  * To run this test suite, start "sbt" then run the "test" command.
  */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /** When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    *   val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val evenNums: FunSet = _ % 2 == 0
    val multsOf4: FunSet = _ % 4 == 0
  }

  /** This test is currently disabled (by using @Ignore) because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", remvoe the
    * @Ignore annotation.
    */
  @Test def `singleton set one contains one`: Unit = {

    /** We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {

      /** The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains elements occurring in both sets`: Unit = {
    new TestSets {
      val s = intersect(evenNums, multsOf4)
      assert(!contains(s, 2))
      assert(contains(s, 4))
    }
  }

  @Test def `diff contains elements in the first but not in the second set`
      : Unit = {
    new TestSets {
      val s = diff(evenNums, multsOf4)
      assert(contains(s, 2))
      assert(!contains(s, 4))
    }
  }

  @Test def `filter removes all elements that do not satisfy the predicate`
      : Unit = {
    new TestSets {
      val s = filter(evenNums, multsOf4)
      assert(!contains(s, 2))
      assert(contains(s, 4))
    }
  }

  @Test def `forall tests if all elements satisfy the predicate`: Unit = {
    new TestSets {
      assert(!forall(evenNums, multsOf4))
      assert(forall(multsOf4, evenNums))
    }
  }

  @Test def `exists tests if at least one element satisfies the predicate`
      : Unit = {
    new TestSets {
      assert(exists(evenNums, multsOf4))
      assert(!exists(evenNums, _ % 2 == 1))
    }
  }

  @Test def `map applies the given function to all elements`: Unit = {
    new TestSets {
      val s = map(evenNums, _ * 2)
      assert(!contains(s, 2), "2 is no longer in the set")
      assert(contains(s, 4), "4 is still in the set")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
