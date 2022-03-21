package fr.arnoux23u.jeudelavie

import scala.annotation.tailrec

object JeuDeLaVie {

  /**
   * Definition du type Grille
   * Une grille est representée par une liste de tuples correpondante aux cellules vivantes
   */
  type Grille = List[(Int, Int)];

  /**
   * Methode permettant de convertir une liste de chaines en grille
   *
   * @param l liste de chaines : ("XXX","X X","  X") par exemple
   * @return objet grille converti
   */
  def chainesToGrille(l: List[String]): Grille = {
    def aux(l: List[String], li: Int): Grille = {
      def f1(str: List[Char], li: Int, col: Int = 0): List[(Int, Int)] = str match {
        case Nil => Nil
        case char :: q => f2(char, li, col) ++ f1(q, li, col + 1)
      }

      def f2(c: Char, li: Int, col: Int): List[(Int, Int)] = if (c == 'X') List((li, col)) else Nil

      l match {
        case Nil => Nil
        case string :: q => f1(string.toList, li) ++ aux(q, li + 1)
      }
    }

    aux(l, 0)
  }

  /**
   * Methode se chargant d'afficher la grille dans le terminal
   *
   * @param g Grille a afficher
   */
  def afficherGrille(g: Grille): Unit = {
    def calculerMinMax(g: Grille): ((Int, Int), (Int, Int)) = {
      (g.reduce((curr, acc) => (Math.min(curr._1, acc._1), Math.min(curr._2, acc._2))), g.reduce((curr, acc) => (Math.max(curr._1, acc._1), Math.max(curr._2, acc._2))))
    }

    @tailrec
    def aux(grille: Grille, t_min: (Int, Int) = null, t_max: (Int, Int) = null, t_pos: (Int, Int) = null): Unit = {
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
        aux(grille, min, max, (pos._1 + 1, pos._2))
      } else if (pos._2 < max._2) {
        print('\n')
        aux(grille, min, max, (min._1, pos._2 + 1))
      } else {
        print('\n')
      }
    }

    aux(g)
  }

  /**
   * Methode permettant de calculer le nombre de cellules communes (meme position) entre deux grilles
   *
   * @param g1 grille 1
   * @param g2 grille 2
   * @return nombre de cellules communes
   */
  def communes(g1: Grille, g2: Grille): Int = g1.count(tuple => g2.contains(tuple))

  /**
   * Methode permettant de recuperer les 4 voisines d'une cellule
   *
   * @param l ligne (pos x)
   * @param c colonne (pos y)
   * @return Liste de tuples, voisines de (l,c)
   */
  def voisines4(l: Int, c: Int): List[(Int, Int)] = (l - 1, c) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c) :: Nil

  /**
   * Methode permettant de recuperer les 4 voisines diagonales d'une cellule
   *
   * @param l ligne (pos x)
   * @param c colonne (pos y)
   * @return Liste de tuples, voisines de (l,c)
   */
  def voisines4Diag(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c + 1) :: (l + 1, c - 1) :: (l + 1, c + 1) :: Nil

  /**
   * Methode permettant de recuperer les 8 voisines d'une cellule
   *
   * @param l ligne (pos x)
   * @param c colonne (pos y)
   * @return Liste de tuples, voisines de (l,c)
   */
  def voisines8(l: Int, c: Int): List[(Int, Int)] = (l - 1, c - 1) :: (l - 1, c) :: (l - 1, c + 1) :: (l, c - 1) :: (l, c + 1) :: (l + 1, c - 1) :: (l + 1, c) :: (l + 1, c + 1) :: Nil

  /**
   * Methode permettant de recuperer les cellules survivantes pour une iteration
   *
   * @param g grille
   * @return grille contenant uniquement les cellules survivantes
   */
  def survivantes(g: Grille): Grille = {
    def aux(g: Grille, origine: Grille = null): Grille = {
      val grilleOrigine = if (origine == null) g else origine
      g match {
        case Nil => Nil
        case (x, y) :: reste => if (List(2, 3).contains(voisines8(x, y).count(tuple => grilleOrigine.contains(tuple)))) (x, y) :: aux(reste, grilleOrigine) else aux(reste, grilleOrigine)
      }
    }

    aux(g)
  }

  /**
   * Methode permettant de recuperer les cellules candidates pour une iteration
   * Les cellules candidates sont des cellules pouvant naitre a l'iteration suivante
   *
   * @param g grille
   * @return grille contenant uniquement les cellules candidates
   */
  def candidates(g: Grille): Grille = {
    def aux(g: Grille, origine: Grille = null): Grille = {
      val grilleOrigine = if (origine == null) g else origine
      g match {
        case Nil => Nil
        case (x, y) :: reste => voisines8(x, y).filter(tuple => !grilleOrigine.contains(tuple)) ++ aux(reste, grilleOrigine)
      }
    }

    aux(g).distinct
  }

  /**
   * Methode permettant de recuperer les cellules naissantes pour une iteration
   *
   * @param g grille
   * @return grille contenant uniquement les cellules naissantes
   */
  def naissances(g: Grille): Grille = {
    candidates(g).filter(candidat => voisines8(candidat._1, candidat._2).count(voisin => g.contains(voisin)) == 3)
  }

  /**
   * Fonction jeudelavie, utilise l'algorithme par defaut
   *
   * @param init grille initiale
   * @param n    nombre d'iterations a effectuer
   */
  def jeuDeLaVie(init: Grille, n: Int): Unit = {
    @tailrec
    def iterer(g: Grille, iter: Int): Unit = {
      if (iter <= n) {
        println("\n---ITERATION " + iter + "---")
        afficherGrille(g)
        val grille = List().appendedAll(survivantes(g)).appendedAll(naissances(g))
        iterer(grille, iter + 1)
      }
    }

    iterer(init, 0)
  }

  /**
   * Fonction globale moteur
   * Permets d'iterer sur le jeu
   *
   * @param init           grille initiale
   * @param n              nb d'iterations
   * @param regleNaissance regle de naissance
   * @param regleSurvie    regle de survie
   * @param voisins        regle de voisinnage
   */
  def moteur(init: Grille, n: Int, regleNaissance: Int => Boolean, regleSurvie: Int => Boolean, voisins: (Int, Int) => List[(Int, Int)]): Unit = {
    @tailrec
    def iterer(g: Grille, iter: Int): Unit = {
      if (iter <= n) {
        println("\n---ITERATION " + iter + "---")
        afficherGrille(g)
        val grille = List().appendedAll(survivantesG(g, regleSurvie, voisins)).appendedAll(naissancesG(g, regleNaissance, voisins))
        iterer(grille, iter + 1)
      }
    }

    iterer(init, 0)
  }

  /**
   * Lancement en mode jeu de la vie
   *
   * @param g grille initiale
   * @param n nombre d'iterations
   */
  def JDLV(g: Grille, n: Int): Unit = {
    println("Lancement du jeu en mode JDLV")
    moteur(g, n, naitJDLV, survitJDLV, voisines8)
  }

  /**
   * Lancement en mode fredkin
   *
   * @param g        grille initiale
   * @param n        nombre d'iterations
   * @param variante booleen. si vrai, variante des voisines diagonales. par defaut a false
   */
  def fredkin(g: Grille, n: Int, variante: Boolean = false): Unit = {
    println("Lancement du jeu en mode FREDKIN")
    moteur(g, n, naitFredkin, survitFredkin, if (variante) voisines4Diag else voisines4)
  }

  /*------GENERALISATION------*/

  /**
   * Methode de paramétrage du jeu de la vie [JDLV]
   * Construit la règle de création des cellules naissantes
   *
   * @param nbVoisines nombre de voisines
   * @return Vrai si naissante, faux sinon
   */
  def naitJDLV(nbVoisines: Int): Boolean = nbVoisines == 3

  /**
   * Methode de paramétrage du jeu de la vie [FREDKIN]
   * Construit la règle de création des cellules naissantes
   *
   * @param nbVoisines nombre de voisines
   * @return Vrai si naissante, faux sinon
   */
  def naitFredkin(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

  /**
   * Methode de paramétrage du jeu de la vie [JDLV]
   * Construit la règle de création des cellules survivantes
   *
   * @param nbVoisines nombre de voisines
   * @return Vrai si survivante, faux sinon
   */
  def survitJDLV(nbVoisines: Int): Boolean = List(2, 3).contains(nbVoisines)

  /**
   * Methode de paramétrage du jeu de la vie [FREDKIN]
   * Construit la règle de création des cellules survivantes
   *
   * @param nbVoisines nombre de voisines
   * @return Vrai si survivante, faux sinon
   */
  def survitFredkin(nbVoisines: Int): Boolean = nbVoisines % 2 == 1

  /**
   * Methode generalisée permettant de recuperer les cellules survivantes pour une iteration
   *
   * @param g        grille
   * @param regle    regle de survie
   * @param voisines calcul de voisinnage
   * @return grille contenant uniquement les cellules survivantes
   */
  def survivantesG(g: Grille, regle: Int => Boolean, voisines: (Int, Int) => List[(Int, Int)]): Grille = {
    def aux(g: Grille, origine: Grille = null): Grille = {
      val grilleOrigine = if (origine == null) g else origine
      g match {
        case Nil => Nil
        case (x, y) :: reste => if (regle(voisines(x, y).count(tuple => grilleOrigine.contains(tuple)))) (x, y) :: aux(reste, grilleOrigine) else aux(reste, grilleOrigine)
      }
    }

    aux(g)
  }

  /**
   * Methode generalisée permettant de recuperer les cellules candidates pour une iteration
   *
   * @param g        grille
   * @param voisines calcul de voisinnage
   * @return grille contenant uniquement les cellules candidates
   */
  def candidatesG(g: Grille, voisines: (Int, Int) => List[(Int, Int)]): Grille = {
    def aux(g: Grille, origine: Grille = null): Grille = {
      val grilleOrigine = if (origine == null) g else origine
      g match {
        case Nil => Nil
        case (x, y) :: reste => voisines(x, y).filter(tuple => !grilleOrigine.contains(tuple)) ++ aux(reste, grilleOrigine)
      }
    }

    aux(g).distinct
  }

  /**
   * Methode generalisée permettant de recuperer les cellules naissantes pour une iteration
   *
   * @param g        grille
   * @param regle    regle de survie
   * @param voisines calcul de voisinnage
   * @return grille contenant uniquement les cellules naissantes
   */
  def naissancesG(g: Grille, regle: Int => Boolean, voisines: (Int, Int) => List[(Int, Int)]): Grille = {
    candidates(g).filter(candidat => regle(voisines(candidat._1, candidat._2).count(voisin => g.contains(voisin))))
  }
  /*------ENDGENERALISATION---*/

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

  def lancerAsserts(g: Grille): Unit = {
    assert(chainesToGrille(listes.head._1).toSet.subsetOf(listes.head._2.toSet))
    assert(chainesToGrille(listes(1)._1) == listes(1)._2)
    assert(chainesToGrille(listes(2)._1) == listes(2)._2)
    assert(chainesToGrille(listes(3)._1) == listes(3)._2)
    //------
    assert(voisines4(2, -1) == List((1, -1), (2, -2), (2, 0), (3, -1)))
    assert(voisines4(0, 0) == List((-1, 0), (0, -1), (0, 1), (1, 0)))
    assert(voisines8(2, -1) == List((1, -2), (1, -1), (1, 0), (2, -2), (2, 0), (3, -2), (3, -1), (3, 0)))
    assert(voisines8(0, 0) == List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)))
    //------DEFAULT
    assert(survivantes(g) == List((1, 1), (2, 0), (2, 2), (3, 1)))
    val cellCandidates = List((-2, 1), (-2, 2), (-2, 3), (-1, 1), (-1, 3), (-1, 4), (0, -3), (0, -2), (0, -1), (0, 0), (0, 1), (0, 2), (0, 4), (1, -3), (1, -1), (1, 0), (1, 2), (1, 3), (1, 4), (2, -3), (2, -2), (2, -1), (2, 1), (2, 3), (3, -1), (3, 0), (3, 2), (3, 3), (4, 0), (4, 1), (4, 2));
    assert(communes(candidates(g), cellCandidates) == cellCandidates.length)
    val cellNaissantes = List((0, 2), (1, 2))
    assert(communes(naissances(g), cellNaissantes) == cellNaissantes.length)
    //------JDLV
    assert(survivantesG(g, survitJDLV, voisines8) == List((1, 1), (2, 0), (2, 2), (3, 1)))
    assert(communes(candidatesG(g, voisines8), cellCandidates) == cellCandidates.length)
    assert(communes(naissancesG(g, naitJDLV, voisines8), cellNaissantes) == cellNaissantes.length)
    //------FREDKIN
    assert(survivantesG(g, survitFredkin, voisines4) == List.empty)
    val cellCandidatesFRED = List((-2, 2), (-1, 1), (-1, 3), (0, -2), (0, 1), (0, 2), (0, 4), (1, -3), (1, -1), (1, 0), (1, 2), (1, 3), (2, -2), (2, -1), (2, 1), (2, 3), (3, 0), (3, 2), (4, 1));
    assert(communes(candidatesG(g, voisines4), cellCandidatesFRED) == cellCandidatesFRED.length)
    val cellNaissantesFRED = List((-2, 2), (-1, 1), (0, -2), (0, 1), (0, 4), (1, -3), (1, -1), (1, 3), (2, -2), (2, -1), (2, 3), (4, 1));
    assert(communes(naissancesG(g, naitFredkin, voisines4), cellNaissantesFRED) == cellNaissantesFRED.length)
    //------
    assert(naitJDLV(3))
    assert(!naitJDLV(2))
    assert(!naitJDLV(4))
    assert(survitJDLV(2))
    assert(survitJDLV(3))
    assert(!survitJDLV(4))
    //------
    assert(survitFredkin(5))
    assert(survitFredkin(3))
    assert(!survitFredkin(6))
    assert(naitFredkin(5))
    assert(naitFredkin(3))
    assert(!naitFredkin(6))
  }
  /*---------ENDOFTEST-----------*/

  /*---------MAIN-----------*/
  def main(args: Array[String]): Unit = {
    val g: Grille = List(
      (-1, 2),
      (0, 3),
      (1, -2),
      (1, 1),
      (2, 0),
      (2, 2),
      (3, 1),
    )
    lancerAsserts(g)
    //Lancement jeu
    fredkin(g, 4000)
    println("---Jeu terminé---")
  }
  /*---------ENDOFMAIN-----------*/

}
