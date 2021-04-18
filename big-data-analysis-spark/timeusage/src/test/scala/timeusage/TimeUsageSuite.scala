package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.junit.{Assert, Test}
import org.junit.Assert.assertEquals
import TimeUsage._

import scala.util.Random

class TimeUsageSuite {
  @Test def `row parses input correctly`: Unit = {
    val res = row(List("foo", "1.5", "2.0"))
    assert(res.equals(Row("foo", 1.5, 2.0)))
  }

  @Test def `time usage`: Unit = {
    val (columns, initDf) = read("src/main/resources/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(
      columns
    )
    val summaryDf =
      timeUsageSummary(
        primaryNeedsColumns,
        workColumns,
        otherColumns,
        initDf
      )
    val summed = timeUsageSummaryTyped(summaryDf)
    val finalDf = timeUsageGroupedTyped(summed)
    finalDf.show()
  }
}
