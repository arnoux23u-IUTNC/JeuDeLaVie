package fr.arnoux23u.jeudelavie

import scala.annotation.tailrec

object JeuDeLaVie {

  type Grille = List[(Int, Int)];

  def chainesToGrille(l: List[String], li: Int = 0): Grille = {
    def f1(str: List[Char], li: Int, col: Int = 0): List[(Int, Int)] = str match {
      case Nil => Nil
      case char :: q => f2(char, li, col) ++ f1(q, li, col + 1)
    }

    def f2(c: Char, li: Int, col: Int): List[(Int, Int)] = if (c == 'X') List((li, col)) else Nil

    l match {
      case Nil => Nil
      case string :: q => f1(string.toList, li) ++ chainesToGrille(q, li + 1)
    }
  }

  def calculerMinMax(g: Grille): ((Int, Int), (Int, Int)) = {
    (g.reduce((curr, acc) => (Math.min(curr._1, acc._1), Math.min(curr._2, acc._2))), g.reduce((curr, acc) => (Math.max(curr._1, acc._1), Math.max(curr._2, acc._2))))
  }

  @tailrec
  def afficherGrille(grille: Grille, t_min: (Int, Int) = null, t_max: (Int, Int) = null, t_pos: (Int, Int) = null): Unit = {
    val coords = calculerMinMax(grille);
    val min = if (t_min == null) coords._1 else t_min;
    val max = if (t_max == null) coords._2 else t_max;
    val pos = if (t_pos == null) min else t_pos;
    if (grille.contains(pos)) {
      print("X")
    } else {
      print("_")
    }
    if (pos._1 < max._1) {
      afficherGrille(grille, min, max, (pos._1+1, pos._2))
    } else if (pos._2 < max._2) {
      print('\n')
      afficherGrille(grille, min, max, (min._1, pos._2+1))
    }
  }

  /*---------TEST-----------*/
  val listes: List[(List[String], List[(Int, Int)])] = List(
    (
      List(
        "XXX",
        "XXX",
        "XXX"
      ),
      List(
        (0, 0),
        (0, 1),
        (0, 2),
        (1, 0),
        (1, 1),
        (1, 2),
        (2, 0),
        (2, 1),
        (2, 2),
      )
    ),
    (
      List(
        " XX",
        "X X",
        "XX "
      ),
      List(
        (0, 1),
        (0, 2),
        (1, 0),
        (1, 2),
        (2, 0),
        (2, 1),
      )
    ),
    (
      List(
        "  X",
        " X",
        "X X"
      ),
      List(
        (0, 2),
        (1, 1),
        (2, 0),
        (2, 2),
      )
    ),
    (
      List(
        "   ",
        "   ",
        "   "
      ),
      List()
    )
  )
  /*---------ENDOFTEST-----------*/

  /*---------MAIN-----------*/
  def main(args: Array[String]): Unit = {
    //TESTS UNITAIRES
    assert(chainesToGrille(listes.head._1).toSet.subsetOf(listes.head._2.toSet))
    assert(chainesToGrille(listes(1)._1) == listes(1)._2)
    assert(chainesToGrille(listes(2)._1) == listes(2)._2)
    assert(chainesToGrille(listes(3)._1) == listes(3)._2)
    val g: Grille = List(
      (-1, 2),
      (1, -2),
      (2, 0),
      (2, 2),
      (3, 2),
    )
    assert(calculerMinMax(g)==((-1,-2),(3,2)))
    afficherGrille(g)
  }
  /*---------ENDOFMAIN-----------*/

}
