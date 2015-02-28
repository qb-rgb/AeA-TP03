object Main {

  def main(args: Array[String]): Unit = {
    val mots: Array[String] = Array(
      "gag", "gai", "gaz", "gel", "gks", "gin",
      "gnu", "glu", "gui", "guy", "gre", "gue",
      "ace", "acm", "agi", "ait", "aie", "ail",
      "air", "and", "alu", "ami", "arc", "are",
      "art", "apr", "avr", "sur", "mat", "mur")

    val nb: Int = mots.length

    val succ: Array[Liste] = new Array[Liste](nb)

    // Initialisation des listes de succésseurs
    for (i <- 0 until nb) {
      succ(i) = ListeVide
    }

    val graphe: Graphe = new Graphe(mots, succ)

    graphe.lettreQuiSaute

    println(graphe.toString)
  }

}
