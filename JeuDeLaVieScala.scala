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

  @tailrec
  def parcourirRectangle(grille: Grille,min: (Int, Int), max: (Int, Int), tmp: (Int, Int) = null): Unit = {
    val pos = if (tmp == null) min else tmp;
    if(grille.contains(pos)){
      print("X")
    }else{
      print("_")
    }
    if (pos._2 < max._2) {
      parcourirRectangle(grille, min, max, (pos._1, pos._2 + 1))
    } else if (pos._1 < max._1) {
      print('\n')
      parcourirRectangle(grille, min, max, (pos._1 + 1, min._2))
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
    assert(chainesToGrille(listes.head._1).toSet.subsetOf(listes.head._2.toSet))
    assert(chainesToGrille(listes(1)._1) == listes(1)._2)
    assert(chainesToGrille(listes(2)._1) == listes(2)._2)
    assert(chainesToGrille(listes(3)._1) == listes(3)._2)
    val grille : Grille = chainesToGrille(listes.head._1)
    val min = (0, 0);
    val max = (6,6);
    parcourirRectangle(grille, min, max);
  }
  /*---------ENDOFMAIN-----------*/

}
