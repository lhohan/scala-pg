object Associations {

  private case class ValidAssociation(child: Node, parent: Node, valid: Boolean = true)

  private type Node = String

  private def findFrom(n: Node) = ???

  def findFrom(start: String, domain: String): String = {
    ""
  }
}