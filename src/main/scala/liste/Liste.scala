/**
 * Classe qui permet de gérer les listes d'entiers
 *
 * @author Quentin Baert
 */

trait Liste {
  
  /**
   * Donne la tête de la liste
   *
   * @return tête de la liste
   */
  def head: Int

  /**
   * Donne la queue de la liste
   *
   * @return queue de la liste
   */
  def tail: Liste

  /**
   * Determine si la liste est vide ou non
   *
   * @return true si la liste est vide, false sinon
   */
  def isEmpty: Boolean

  /**
   * Donne la taille de la liste
   */
  def length: Int

  /**
   * Ajoute un élément en queue de liste
   *
   * @param elem
   *          élément à ajouter queue de liste
   * @return nouvelle liste avec l'élément ajouté en queue
   */
  def add(elem: Int): Liste

  /**
   * Renvoit le i-ème élément de la liste
   *
   * @param index
   *          indice de l'élément à renvoyer
   * @return i-ème élément de la liste
   */
  def get(index: Int): Int

  /**
   * Determine si la liste contient un élément
   *
   * @param elem
   *          élément dont on souhaite savoir s'il est dans la liste
   * @return true si la liste contient l'élément, false sinon
   */
  def contains(elem: Int): Boolean
  
}

case object ListeVide extends Liste {

  /**
   * @see liste.Liste#head
   */
  override def head: Int = throw new Error("ListeVide.head")

  /**
   * @see liste.Liste#tail
   */
  override def tail: Liste = throw new Error("ListeVide.tail")

  /**
   * @see liste.Liste#isEmpty
   */
  override def isEmpty: Boolean = true

  /**
   * @see liste.Liste#length
   */
  override def length: Int = 0

  /**
   * @see liste.Liste#add
   */
  override def add(elem: Int): ListeEntiers =
    new ListeEntiers(elem)

  /**
   * @see Liste.liste#get
   */
  override def get(index: Int) = throw new Error("ListeVide.get")

  /**
   * @see Liste.liste#contains
   */
  override def contains(elem: Int) = false

}

case class ListeEntiers(private val headConst: Int, private val tailConst: Liste) extends Liste {

  /**
   * Second constructeur qui permet de créer une liste uniquement à partir
   * d'un entier
   *
   * @param head
   *          entier à partir duquel créer la liste
   */
  def this(head: Int) =
    this(head, ListeVide)

  /**
   * @see liste.Liste#head
   */
  override def head: Int = this.headConst

  /**
   * @see liste.Liste#tail
   */
  override def tail: Liste = this.tailConst

  /**
   * @see liste.Liste#isEmpty
   */
  override def isEmpty: Boolean = false

  /**
   * @see liste.Liste#length
   */
  override def length: Int =
    1 + this.tail.length

  /**
   * @see liste.Liste#add
   */
  override def add(elem: Int): Liste = {
    val newElem = new ListeEntiers(elem)
    if (this.tail.isEmpty)
      new ListeEntiers(this.head, new ListeEntiers(elem))
    else
      new ListeEntiers(this.head, this.tail.add(elem))
  }

  /**
   * @see liste.Liste#get
   */
  override def get(index: Int): Int =
    if (index >= this.length)
      throw new Error("ListeEntiers.get : index out of bounds")
    else if (index == 0)
      this.head
    else
      this.tail.get(index - 1)

  /**
   * @see Liste.liste#contains
   */
  override def contains(elem: Int): Boolean =
    if (this.head == elem)
      true
    else
      this.tail contains elem

}

