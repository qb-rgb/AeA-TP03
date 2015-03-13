/**
 * Objet de tests pour la classe Graphe
 *
 * La classe Dicos est utilisée, il faut donc passer un entier compris entre
 * 3 et 5 pour déterminer le dictionnaire utilisé à la construction du graphe
 *
 * @author Quentin Baert
 */
object Main {

  private val dico3court: Array[String] = Array(
    "gag", "gai", "gaz", "gel", "gks", "gin",
    "gnu", "glu", "gui", "guy", "gre", "gue",
    "ace", "acm", "agi", "ait", "aie", "ail",
    "air", "and", "alu", "ami", "arc", "are",
    "art", "apr", "avr", "sur", "mat", "mur")

  def main(args: Array[String]): Unit = {
    val dico = args(0).toInt

    if ((2 to 5) contains dico) {
      val mots: Array[String] = 
        if (dico == 2)
          this.dico3court
        else if (dico == 3)
          Dicos.dico3.distinct
        else if (dico == 4)
          Dicos.dico4.distinct
        else
          Dicos.dico5.distinct

      val nb: Int = mots.length

      val succ: Array[Liste] = new Array[Liste](nb)

      // Initialisation des listes de succésseurs
      for (i <- 0 until nb)
        succ(i) = ListeVide

      val graphe: Graphe = new Graphe(mots, succ)

      graphe.lettreQuiSaute

      println("\n===== Graphe =====\n")
      println(graphe.toString)

      println("\n===== Composantes connexes du graphe =====\n")
      graphe.visit

      if (dico == 4) {
        println("\n===== Composante connexe des mots lion et peur =====\n")
        graphe dfs "lion"
        println()
        println("\n===== Chemin entre lion et peur =====\n")
        graphe.chemin("lion", "peur")
        println()
      }

    } else
      println("ERREUR : Veuillez entrer un chiffre entre 2 et 5")
  }

}
