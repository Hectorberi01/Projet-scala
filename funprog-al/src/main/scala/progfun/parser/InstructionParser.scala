package fr.esgi.al.funprog.parser
import fr.esgi.al.funprog.model.*
import scala.util.Try

final case class Instruction(
    temps: Int,
    action: Action,
    couleur: Color,
    cibles: Set[Coord])
object InstructionParser:

  def parseCoord(s: String | Null): Try[Coord] = {
    for {
      safe    <- Try(s.nn)
      trimmed <- Try(safe.trim.nn)
      parts   <- Try(trimmed.split(",").nn.map(_.nn))
      xstr    <- Try(parts.lift(0).getOrElse("")).map(_.toInt)
      ystr    <- Try(parts.lift(1).getOrElse("")).map(_.toInt)
    } yield Coord(xstr, ystr)
  }

  def parsePlage(s: String): Try[Set[Coord]] =
    val parts = s.nn.split("-").nn.map(_.nn.trim)
    if parts.length == 1 then parseCoord(parts(0)).map(Set(_))
    else
      for
        debut <- parseCoord(parts(0))
        fin   <- parseCoord(parts(1))
      yield {
        (for {
          x <- debut.x to fin.x
          y <- debut.y to fin.y
        } yield Coord(x, y)).toSet
      }

  def parseCouleur(s: String | Null): Try[Color] =
    for {
      safe    <- Try(s.nn)
      trimmed <- Try(safe.trim.nn)
      parts   <- Try(trimmed.split(",").nn.map(_.nn.toInt))
      either <- Try {
        Color.fromRGB(parts(0), parts(1), parts(2)).toRight("Couleur invalide")
      }
      color <- Try(either match
        case Right(c)  => c
        case Left(msg) => sys.error(msg)
      )
    } yield color

  def parseLigne(ligne: String | Null): Try[Instruction] =
    for {
      safe    <- Try(ligne.nn)
      parties <- Try(safe.split('|').nn.map(_.nn.trim))
      _       <- Try(parties.length).filter(_ == 4)
      temps   <- Try(parties(0).nn.toInt)
      action <- Try {
        Action.fromString(parties(1).nn).getOrElse(sys.error("Action invalide"))
      }
      couleur <- parseCouleur(parties(2))
      cibles  <- parsePlage(parties(3).nn)
    } yield Instruction(temps, action, couleur, cibles)
