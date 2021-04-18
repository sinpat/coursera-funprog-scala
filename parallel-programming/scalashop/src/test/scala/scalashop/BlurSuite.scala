package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  trait TestHelpers {
    val emptyImage = new Img(0, 0, Array.empty)
    val pixel1 = rgba(255, 255, 255, 255)

    val minimumImage = new Img(1, 1, Array(pixel1))
  }
  @Test def `kernel returns 0 for out of range coords`: Unit =
    new TestHelpers {
      assertEquals(0, boxBlurKernel(emptyImage, 1, 1, 1))
    }

  @Test def `kernel for radius 0`: Unit = {
    new TestHelpers {
      assertEquals(
        pixel1,
        boxBlurKernel(minimumImage, 0, 0, 0)
      )
    }
  }

  @Test def `kernel clamps`: Unit = {
    new TestHelpers {
      assertEquals(
        pixel1,
        boxBlurKernel(minimumImage, 0, 0, 10)
      )
    }
  }

  @Test def `kernel works`: Unit = {
    val `100` = rgba(100, 100, 100, 100)
    val `200` = rgba(200, 200, 200, 200)
    assertEquals(
      rgba(150, 150, 150, 150),
      boxBlurKernel(new Img(2, 1, Array(`100`, `200`)), 0, 0, 1)
    )
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
