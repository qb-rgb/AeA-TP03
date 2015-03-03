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
  
  // Les deux tableaux passés à la construction doivent être de même taille
  require(this.mots.length == this.listeSucc.length)

  // Nombre de mots
  private val nb: Int = this.mots.length

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
  
  private def dfsWithTab(mot: Int, tab: Array[Boolean]): Unit = {
    tab(mot) = true
    print(this.mots(mot) + "  ")

    val succ = this.listeSucc(mot)

    for (i <- 0 until succ.length) {
      val m = succ.get(i) 
      if (!tab(m))
        dfsWithTab(m, tab)
    }

  }

  def dfs(mot: Int): Unit = {
    val tagged: Array[Boolean] = new Array(this.nb)

    // Initialisation du tableau de booléens
    for (i <- 0 until this.nb)
      tagged(i) = false

    
    this.dfsWithTab(mot, tagged)
  }

  def dfs(mot: String): Unit =
    if (this.mots contains mot) {
      val indexedWords = this.mots.zipWithIndex
      val index = (indexedWords filter (x => x._1 == mot)).head._2

      this.dfs(index)
    }
    else
      throw new Error("Graphe.dfs: mot " + mot + " introuvable")


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

}
