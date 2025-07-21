package fr.esgi.al.funprog

object Partie2 {

  def nbPossibilites(n: Int, m: Int): Int = {
    val grid = Array.fill(n, m)(false)

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def backtrack(pos: Int): Int = {
      if (pos == n * m) 1
      else {
        val x = pos / m
        val y = pos % m

        if (grid(x)(y))
          backtrack(pos + 1)
        else {
          val horizontal =
            if (y + 1 < m && !grid(x)(y + 1)) {
              grid(x)(y) = true
              grid(x)(y + 1) = true
              val res = backtrack(pos + 1)
              grid(x)(y) = false
              grid(x)(y + 1) = false
              res
            } else 0

          val vertical =
            if (x + 1 < n && !grid(x + 1)(y)) {
              grid(x)(y) = true
              grid(x + 1)(y) = true
              val res = backtrack(pos + 1)
              grid(x)(y) = false
              grid(x + 1)(y) = false
              res
            } else 0

          horizontal + vertical
        }
      }
    }

    backtrack(0)
  }

  /** Point d'entrée principal compatible SBT */
  def main(args: Array[String]): Unit = {
    val n = 2
    val m = 2
    val resultat = nbPossibilites(n, m)
    println(s"Possibilités d'affichage pour le panneau $n x $m: $resultat")
  }
}
