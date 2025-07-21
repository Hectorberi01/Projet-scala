//package example.Partie2Spec

package fr.esgi.al.funprog

import munit.FunSuite

class Partie2Spec extends FunSuite {

  test("2x2 : il existe 2 possibilités") {
    val result = Partie2.nbPossibilites(2, 2)
    assertEquals(result, 2)
  }

  test("2x3 : il existe 3 possibilités") {
    val result = Partie2.nbPossibilites(2, 3)
    assertEquals(result, 3)
  }

  test("4x4 : il existe 36 possibilités") {
    val result = Partie2.nbPossibilites(4, 4)
    assertEquals(result, 36)
  }

  test("5x5 : impossible, résultat = 0") {
    val result = Partie2.nbPossibilites(5, 5)
    assertEquals(result, 0)
  }

  test("1x3 : impossible, résultat = 0") {
    val result = Partie2.nbPossibilites(1, 3)
    assertEquals(result, 0)
  }

  test("2x1 : il existe 1 possibilité") {
    val result = Partie2.nbPossibilites(2, 1)
    assertEquals(result, 1)
  }
}
