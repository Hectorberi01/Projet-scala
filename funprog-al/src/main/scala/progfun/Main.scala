package fr.esgi.al.funprog
import fr.esgi.al.funprog.model.{Action, LED, Panneau}
import fr.esgi.al.funprog.parser.{Instruction, InstructionParser}

import java.nio.charset.StandardCharsets
import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}

@main
def Main(): Unit = {

  val cheminFichier = "/Users/hector/Downloads/resetCode.txt"

  // val lignes = Try(Source.fromFile(cheminFichier).getLines().toList)
  // ✅ Codecs explicite
  implicit val codec: Codec = Codec(StandardCharsets.UTF_8.nn)

  val lignes = Try(Source.fromFile(cheminFichier).getLines().toList)
  lignes match
    case Failure(e) =>
      println(s"Erreur lors de la lecture du fichier : ${e.getMessage}")

    case Success(lignesFichier) =>
      lignesFichier match
        case Nil =>
          println("Fichier vide.")

        case tailleStr :: instructions =>
          val tailleTry = Try {
            val Array(nStr, mStr) = tailleStr.trim.nn.split(" ").nn
            (nStr.nn.toInt, mStr.nn.toInt)
          }

          tailleTry match
            case Failure(e) =>
              println(s"Erreur dans la ligne de taille : ${e.getMessage}")

            case Success((n, m)) =>
              // println(s"taille : $n x $m\n")

              val instructionsParsees: List[Instruction] =
                instructions.zipWithIndex.flatMap { case (ligne, i) =>
                  InstructionParser.parseLigne(ligne) match
                    case Success(instr) =>
                      // println(s"[OK] Ligne ${i + 2} : $instr")
                      Some(instr)
                    case Failure(e) =>
                      // println(s"[ERREUR] Ligne ${i + 2} : ${e.getMessage}")
                      None
                }

              instructionsParsees.foreach(println)
              val panneauInitial = Panneau.vide(n, m)
              val panneauFinal = instructionsParsees
                .sortBy(_.temps)
                .foldLeft(panneauInitial)(appliquerInstruction)

              val maxTemps =
                instructionsParsees.map(_.temps).maxOption.getOrElse(0)

              println()
              println(s"- taille: ${n}x${m}")
              println("- couleurs:")
              panneauFinal.couleursActuelles.toSeq
                .sortBy(_._1)
                .foreach { case (c, count) => println(s"  - $c: $count") }

              println(s"- cumul: ${panneauFinal.tempsAllumeCumule(maxTemps)}")

  println("Ici le programme principal")
  println(Console.BLUE + "x")
  println(Console.RED + "x")
  println(Console.WHITE + "x")
  println(Console.GREEN + "x")

  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
}

def appliquerInstruction(
    panneau: Panneau,
    instruction: Instruction): Panneau = {
  val t = instruction.temps
  val action = instruction.action
  val nouvelleCouleur = instruction.couleur

  instruction.cibles.foldLeft(panneau) { (p, coord) =>
    p.get(coord) match
      case Some(led) =>
        val ledMaj = action match
          case Action.Incrementer =>
            if (led.intensite >= 1.0) led
            else {
              val intensiteMaj = (led.intensite + 1.0).min(1.0)
              val colorMaj = if (led.estEteinte) nouvelleCouleur else led.color
              val newLed = LED(colorMaj, intensiteMaj, led.allumeDepuis)
              if (intensiteMaj > 0.0 && led.allumeDepuis.isEmpty)
                newLed.copy(allumeDepuis = Some(t))
              else if (intensiteMaj == 0.0)
                newLed.copy(allumeDepuis = None)
              else newLed
            }

          case Action.Decrementer =>
            if (led.intensite <= 0.0) led
            else {
              val intensiteMaj = (led.intensite - 1.0).max(0.0)
              val newLed = led.copy(intensite = intensiteMaj)
              if (intensiteMaj == 0.0) newLed.copy(allumeDepuis = None)
              else newLed
            }

          case Action.Switch =>
            val newIntensite = if (led.intensite == 0.0) 1.0 else 0.0
            val colorMaj = if (led.estEteinte) nouvelleCouleur else led.color
            val newLed = LED(colorMaj, newIntensite, led.allumeDepuis)
            if (newIntensite == 0.0) newLed.copy(allumeDepuis = None)
            else if (led.allumeDepuis.isEmpty)
              newLed.copy(allumeDepuis = Some(t))
            else newLed

        p.maj(coord, ledMaj)

      case None => p
  }
}
