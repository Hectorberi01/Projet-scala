package fr.esgi.al.funprog.model

final case class Panneau(n: Int, m: Int, leds: Map[Coord, LED]) {
  def get(coord: Coord): Option[LED] = leds.get(coord)

  def maj(coord: Coord, led: LED): Panneau =
    if (coord.x >= 0 && coord.x < n && coord.y >= 0 && coord.y < m)
      copy(leds = leds.updated(coord, led))
    else this // ou retourner une erreur plus tard

  def couleursActuelles: Map[(Int, Int, Int), Int] =
    leds.values
      .filter(_.estAllumee)
      .groupBy(_.color.rgb)
      .view
      .mapValues(_.size)
      .toMap

  def tempsAllumeCumule(fin: Int): Int =
    leds.values.collect {
      case led if led.estAllumee && led.allumeDepuis.nonEmpty =>
        fin - led.allumeDepuis.fold(0)(identity) // ou use `map + getOrElse`
    }.sum
}

object Panneau {
  def vide(n: Int, m: Int): Panneau = {
    val toutes = for {
      x <- 0 until n
      y <- 0 until m
    } yield Coord(x, y) -> LED(Color.Noir, 0.0, None)

    Panneau(n, m, toutes.toMap)
  }

}
