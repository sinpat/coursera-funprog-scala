package funsets

object Main extends App {
  val r = (1 to 10000000).toList
  val five = r.view
    .map(_ + 1)
    .map(_ + 1)
    .map(_ + 1)
    .map(_ + 1)
    .map(_ + 1)
    .take(5)
    .toList
  println(five)
}
