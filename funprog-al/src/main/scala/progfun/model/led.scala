package fr.esgi.al.funprog.model

final case class LED(
    color: Color,
    intensite: Double,
    allumeDepuis: Option[Int]) {
  require(intensite >= 0.0 && intensite <= 1.0, "Intensité invalide")

  def estAllumee: Boolean = color match {
    case Color.Noir => false
    case _          => intensite > 0.0
  }

  def estEteinte: Boolean = color match {
    case Color.Noir => true
    case _          => intensite <= 0.0
  }

  def avecAllumage(t: Int): LED =
    if (estAllumee && allumeDepuis.isEmpty) copy(allumeDepuis = Some(t))
    else this

  def avecExtinction: LED =
    if (estEteinte) copy(allumeDepuis = None)
    else this
}
