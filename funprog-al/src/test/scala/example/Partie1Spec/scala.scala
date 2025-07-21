package fr.esgi.al.funprog

import scala.util.{Failure, Success}
import munit.FunSuite
import fr.esgi.al.funprog.model.{Color, LED, Panneau}
import fr.esgi.al.funprog.parser.{Instruction, InstructionParser}
//import scala.util.Success

class Partie1Spec extends FunSuite {

  test("Création d'un panneau vide") {
    val p = Panneau.vide(5, 5)
    assertEquals(p.n, 5)
    assertEquals(p.m, 5)
    assertEquals(p.leds.size, 25)
  }

  test("LED éteinte") {
    val led = LED(Color.Noir, 0.0, None)
    assert(!led.estAllumee)
    assert(led.estEteinte)
  }

  test("Parsing d'une instruction valide") {
    val ligne = "1 | + | 1,0,0 | 0,0 - 1,0"
    val result = InstructionParser.parseLigne(ligne)
    result match {
      case Success(instr) => assertEquals(instr.temps, 1)
      case Failure(e)     => fail(s"Parsing a échoué : ${e.getMessage}")
    }
  }

  test("Simulation d'une instruction") {
    val p = Panneau.vide(2, 2)
    val instr = Instruction(
      1,
      model.Action.Incrementer,
      Color.Rouge,
      Set(model.Coord(0, 0))
    )
    val p2 = appliquerInstruction(p, instr)
    assert(p2.get(model.Coord(0, 0)).exists(_.estAllumee))
  }
}
