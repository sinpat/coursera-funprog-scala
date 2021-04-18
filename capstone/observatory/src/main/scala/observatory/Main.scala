package observatory

object Main extends App {
  println(Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv"))
}
