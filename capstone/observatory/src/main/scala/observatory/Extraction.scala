package observatory

import java.time.LocalDate
import scala.io.Source

/** 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  /** @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(
      year: Year,
      stationsFile: String,
      temperaturesFile: String
  ): Iterable[(LocalDate, Location, Temperature)] = {
    def parseCSV(path: String): Iterator[Array[String]] = Source
      .fromInputStream(getClass.getResourceAsStream(path), "utf-8")
      .getLines()
      .map(_.split(','))

    val stationMap: Map[StationID, Location] = parseCSV(stationsFile)
      .withFilter {
        case Array(_, _2, lat, lng) if (lat != "" && lng != "") => true
        case _                                                  => false
      }
      .map { case Array(stn, wban, lat, lng) =>
        (StationID(stn, wban), Location(lat.toDouble, lng.toDouble))
      }
      .withFilter { case (_, loc) =>
        loc.lat != 0 && loc.lon != 0
      }
      .toMap

    parseCSV(temperaturesFile).toIterable
      .filter(_.length == 5)
      .map { case Array(stn, wban, month, day, tempInF) =>
        stationMap
          .get(StationID(stn, wban))
          .map(
            (
              LocalDate.of(year, month.toInt, day.toInt),
              _,
              (tempInF.toDouble - 32) / 1.8
            )
          )
      }
      .collect { case Some(x) => x }
      .toIterable
  }

  /** @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(
      records: Iterable[(LocalDate, Location, Temperature)]
  ): Iterable[(Location, Temperature)] =
    records
      .groupBy(_._2)
      .mapValues(els => els.foldLeft(0.0)(_ + _._3) / els.size)
      .toIterable
}
