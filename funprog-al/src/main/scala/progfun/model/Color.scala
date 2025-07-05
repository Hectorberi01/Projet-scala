package fr.esgi.al.funprog.model

enum Color(val rgb: (Int, Int, Int)) {
  case Noir extends Color(0, 0, 0)
  case Rouge extends Color(1, 0, 0)
  case Vert extends Color(0, 1, 0)
  case Bleu extends Color(0, 0, 1)
  case Blanc extends Color(1, 1, 1)
}

object Color {
  def fromRGB(r: Int, g: Int, b: Int): Option[Color] = (r, g, b) match {
    case (0, 0, 0) => Some(Color.Noir)
    case (1, 0, 0) => Some(Color.Rouge)
    case (0, 1, 0) => Some(Color.Vert)
    case (0, 0, 1) => Some(Color.Bleu)
    case (1, 1, 1) => Some(Color.Blanc)
    case _         => None
  }
}
