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
    if (!(this.listeSucc(m1) contains m2))
    if (this.diffUneLettre(this.mots(m1), this.mots(m2)))
  } this.ajouterArete(m1, m2)

  /**
   * Donne une représentation du graphe
   *
   * @return représentation du graphe sous forme d'une chaine de caractères
   */
  override def toString: String = ""

}
