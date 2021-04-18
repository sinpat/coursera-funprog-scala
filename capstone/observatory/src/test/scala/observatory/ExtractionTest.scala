package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  @Test def works() = assertEquals(
    (),
    Extraction.locateTemperatures(2020, "/stations.csv", "/2020.csv")
  )

}
