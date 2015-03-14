import scala.collection.mutable.Queue

/**
 * Classe représentant un grapge initialisé selon la règle du jeu "la lettre
 * qui saute"
 *
 * @constructor Crée un graphe non initialisé
 *
 * @param mots
 *          mots représentant les noeuds du graphe
 * @param listeSucc
 *          liste donnant les indices des successeurs d'un mot
 *
 * @author Quentin Baert
 */
class Graphe(val mots: Array[String], val listeSucc: Array[Liste]) {

  /////////////////////////////////////////
  // NÉCÉSSAIRE À LA CRÉATION DE L'OBJET //
  /////////////////////////////////////////

  // Les deux tableaux passés à la construction doivent être de même taille
  require(this.mots.length == this.listeSucc.length)

  ///////////////
  // ATTRIBUTS //
  ///////////////

  // Nombre de mots
  private val nb: Int = this.mots.length

  // Tableaux des peres : pere[i] donne l'indice du pere du mot d'indice i
  private val pere: Array[Int] = {
    val res = new Array[Int](this.nb)
    for (i <- 0 until this.nb)
      res(i) = -1

    res
  }

  //////////////
  // MÉTHODES //
  //////////////

  /**
   * Ajoute une arête au graphe
   *
   * @param mot1
   *          première extrémité de l'arête ajoutée
   * @param mot2
   *          seconde extrémité de l'arête ajoutée
   */
  def ajouterArete(mot1: Int, mot2: Int): Unit = {
    val succMot1 = this.listeSucc(mot1).add(mot2)
    val succMot2 = this.listeSucc(mot2).add(mot1)
    this.listeSucc(mot1) = succMot1
    this.listeSucc(mot2) = succMot2
  }

  // Determine si deux mots diffèrent d'une lettre
  private def diffUneLettre(mot1: String, mot2: String): Boolean =
    if (mot1.length == mot2.length) {
      // Compte le nombre de lettre différentes dans les deux mots
      val diffLettersNb = (mot1 zip mot2) count (x => x._1 != x._2)
      diffLettersNb == 1
    }
    // Il faut que les mots aient la même taille
    else
      false

  /**
   * Construit le graphe selon la règle du jeu "la lettre qui saute"
   */
  def lettreQuiSaute: Unit = for {
    m1 <- 0 until this.nb
    m2 <- 0 until this.nb
    // L'arête n'est créee que si elle n'existe pas
    if (!(this.listeSucc(m1) contains m2))
    if (this.diffUneLettre(this.mots(m1), this.mots(m2)))
  } this.ajouterArete(m1, m2)

  // Donne l'indice d'un mot du graphe
  private def wordToIndex(word: String): Int =
    if (this.mots contains word) {
      val indexedWords = this.mots.zipWithIndex
      val index = (indexedWords filter (x => x._1 == word)).head._2

      index
    }
    else
      throw new Error("Graphe: mot " + word + " introuvable")

  /**
   * Donne une représentation du graphe
   *
   * @return représentation du graphe sous forme d'une chaine de caractères
   */
  override def toString: String = {
    def edgesToString(wordIndex: Int): String = {
      val word = this.mots(wordIndex)
      val succ = this.listeSucc(wordIndex)

      val edges =
        for (i <- 0 until succ.length)
          yield word + " -> " + this.mots(succ.get(i))

      edges mkString "\n"
    }

    val allEdges = for (i <- 0 until this.nb) yield edgesToString(i)

    (allEdges filterNot (x => x.isEmpty)) mkString "\n----\n"
  }

  ////////////////////////////////////
  // PARCOURS EN PROFONDEUR D'ABORD //
  ////////////////////////////////////

  // Exécute un parcours en profondeur d'abord sur la graphe à partir d'un mot
  private def dfsWithTab(mot: Int, tab: Array[Boolean], doPrint: Boolean): Unit = {
    tab(mot) = true

    if (doPrint)
      print(this.mots(mot) + "  ")

    val succ = this.listeSucc(mot)

    for (i <- 0 until succ.length) {
      val m = succ get i

      if (!tab(m)) {
        // Initialisation du père
        this.pere(m) = mot
        dfsWithTab(m, tab, doPrint)
      }
    }
  }

  // DFS générique
  private def dfsGeneric(mot: Int, doPrint: Boolean) = {
    val tagged: Array[Boolean] = new Array(this.nb)

    // Initialisation du tableau de booléens
    for (i <- 0 until this.nb)
      tagged(i) = false

    this.dfsWithTab(mot, tagged, doPrint)
  }

  /**
   * Exécute un parcours en profondeur d'abord sur le graphe à partir d'un mot.
   *
   * @param mot indice du mot à partir duquel faire le parcours en profondeur
   */
  def dfs(mot: Int): Unit =
    this.dfsGeneric(mot, false)

  /**
   * Exécute un parcours en profondeur d'abord sur le graphe à partir d'un mot.
   *
   * @param mot mot à partir duquel faire le parcours en profondeur
   */
  def dfs(mot: String): Unit =
    this.dfs(this.wordToIndex(mot))

  /**
   * Exécute un parcours en profondeur d'abord sur le graphe à partir d'un mot.
   * Les mots traversés lors du parcours sont imprimés.
   *
   * @param mot indice du mot à partir duquel faire le parcours en profondeur
   */
  def dfsPrint(mot: Int): Unit =
    this.dfsGeneric(mot, true)

  /**
   * Exécute un parcours en profondeur d'abord sur le graphe à partir d'un mot.
   * Les mots traversés lors du parcours sont imprimés.
   *
   * @param mot mot à partir duquel faire le parcours en profondeur
   */
  def dfsPrint(mot: String): Unit =
    this.dfsPrint(this.wordToIndex(mot))

  /////////////////////////////////
  // PARCOURS EN LARGEUR D'ABORD //
  /////////////////////////////////

  // Exécute un parcours en largeur d'abord sur la graphe à partir d'un mot
  private def bfsWithTab(queue: Queue[Int], tab: Array[Boolean], doPrint: Boolean): Unit = {
    if (!queue.isEmpty) {
      val mot = queue.dequeue

      if (!tab(mot)) {
        tab(mot) = true

        if (doPrint)
          print(this.mots(mot) + "  ")

        val succ = this.listeSucc(mot)

        for (i <- 0 until succ.length) {
          val m = succ get i
          if (!tab(m)) {
            this.pere(m) = mot
            queue += m
          }
        }
      }

      bfsWithTab(queue, tab, doPrint)
    }
  }

  // BFS générique
  private def bfsGeneric(mot: Int, doPrint: Boolean) = {
    val tagged: Array[Boolean] = new Array(this.nb)

    // Initialisation du tableau de booléens
    for (i <- 0 until this.nb)
      tagged(i) = false

    val queue: Queue[Int] = Queue()
    queue += mot

    this.bfsWithTab(queue, tagged, doPrint)
  }

  /**
   * Exécute un parcours en largeur d'abord sur le graphe à partir d'un mot.
   *
   * @param mot indice du mot à partir duquel faire le parcours en largeur
   */
  def bfs(mot: Int): Unit =
    this.bfsGeneric(mot, false)

  /**
   * Exécute un parcours en largeur d'abord sur le graphe à partir d'un mot.
   * Les mots traversés lors du parcours sont imprimés.
   *
   * @param mot indice du mot à partir duquel faire le parcours en largeur
   */
  def bfsPrint(mot: Int): Unit =
    this.bfsGeneric(mot, true)

  /**
   * Exécute un parcours en largeur d'abord sur le graphe à partir d'un mot.
   *
   * @param mot mot à partir duquel faire le parcours en largeur
   */
  def bfs(mot: String): Unit =
    this.bfs(this.wordToIndex(mot))

  /**
   * Exécute un parcours en largeur d'abord sur le graphe à partir d'un mot.
   * Les mots traversés lors du parcours sont imprimés.
   *
   * @param mot mot à partir duquel faire le parcours en largeur
   */
  def bfsPrint(mot: String): Unit =
    this.bfsPrint(this.wordToIndex(mot))

  //////////////////////
  // VISITE DU GRAPHE //
  //////////////////////

  /**
   * Visite le graphe et affiche toutes ses composantes connexes
   */
  def visit: Unit = {
    def indexVisit(index: Int, mot: Int, tab: Array[Boolean]): Unit = {
      print(index + ":  ")
      this.dfsWithTab(mot, tab, true)
      println()
      if (tab exists (x => x == false)) {
        val tabWithIndexes = tab.zipWithIndex
        val m = (tabWithIndexes filter (x => x._1 == false)).head._2

        indexVisit(index + 1, m, tab)
      }
    }

    val tagged: Array[Boolean] = new Array(this.nb)

    // Initialisation du tableau de booléens
    for (i <- 0 until this.nb)
      tagged(i) = false

    indexVisit(0, 0, tagged)
  }

  //////////////////////////////////////
  // CHEMIN ENTRE DEUX MOTS DU GRAPHE //
  //////////////////////////////////////

  def chemin(from: String, to: String, parcours: String => Unit): Unit = {
    // Construit la liste de mot qui compose le chemin entre from et to
    def buildPath(current: Int, path: List[String]): List[String] = {
      val currentWord = this.mots(current)
      if (currentWord == from)
        path
      else {
        val pere = this.pere(current)
        if (pere != -1)
          buildPath(pere, currentWord +: path)
        else
          throw new Error(
            "Graphe.cheminDfs: chemin entre " + from + " et " + to + " inexistant"
          )
      }
    }

    /*
     * Un parcours est fait pour être certain que le tableau pere est
     * correctement initialisé
     */
    parcours(from)

    val toIndex = this.wordToIndex(to)
    val path = buildPath(toIndex, Nil)

    print(from + " -> ")
    println(path mkString " -> ")
  }

  /**
   * Imprime le chemin du graphe entre deux mots en utilisant un DFS
   *
   * @param from mot à partir duquel démarre le chemin
   * @param to mot auquel termine le chemin
   */
  def cheminDfs(from: String, to: String): Unit =
    this.chemin(from, to, this.dfs)

  /**
   * Imprime le chemin du graphe entre deux mots en utilisant un DFS
   *
   * @param from mot à partir duquel démarre le chemin
   * @param to mot auquel termine le chemin
   */
  def cheminBfs(from: String, to: String): Unit =
    this.chemin(from, to, this.bfs)

}
