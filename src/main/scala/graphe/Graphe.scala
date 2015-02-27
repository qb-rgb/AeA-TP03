class Graphe(val mots: Array[String], val listeSucc: Array[Liste]) {
  
  // Nombre de mots
  private val nb: Int = mots.length

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

}
