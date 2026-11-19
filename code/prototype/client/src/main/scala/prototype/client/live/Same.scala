package prototype.client.live

/** What differs between the browser's value and the server's: one item per id
  * that differs for a list (`Same.byId`), or one item for the whole value
  * otherwise (`Same.equal`). Each item keeps both versions as text, so the
  * status stays free of the value's type.
  */
final case class Diff(items: List[Diff.Item]):

  /** The ids found only in the browser. */
  def onlyLocal: List[String] = items.collect {
    case Diff.Item(id, Some(_), None) => id
  }

  /** The ids found only on the server. */
  def onlyServer: List[String] = items.collect {
    case Diff.Item(id, None, Some(_)) => id
  }

  /** The ids found on both sides with different values. */
  def different: List[String] = items.collect {
    case Diff.Item(id, Some(_), Some(_)) => id
  }

  /** A short text, a few ids per part. */
  def summary: String =
    List(
      "only here" -> onlyLocal,
      "only on the server" -> onlyServer,
      "different" -> different
    ).collect {
      case (label, ids) if ids.nonEmpty =>
        val shown = ids.take(Diff.ShownIds).mkString(", ")
        val more = ids.size - Diff.ShownIds
        if more > 0 then s"$label: $shown and $more more"
        else s"$label: $shown"
    }.mkString("; ")

object Diff:
  val ShownIds = 3

  /** One id that differs, with its version on each side as text (None: missing
    * on that side). Finding what differs between the two is left to the reader.
    */
  final case class Item(
      id: String,
      local: Option[String],
      server: Option[String]
  )

  /** A value compared whole: one item with both versions. */
  def whole(local: Any, server: Any): Diff =
    Diff(
      List(Item("the whole value", Some(local.toString), Some(server.toString)))
    )
end Diff

/** How a live value is compared with the server's: None when they match,
  * otherwise what differs.
  */
trait Same[A]:
  def diff(local: A, server: A): Option[Diff]

object Same:

  /** Compared whole, with `==`. For a list, the order counts. */
  def equal[A]: Same[A] = new Same[A]:
    def diff(local: A, server: A): Option[Diff] =
      Option.when(local != server)(Diff.whole(local, server))

  /** A list compared item by item, by `id`: the order of the list does not
    * count, the items' values do. An id found twice on one side (an event
    * applied twice) is reported as different, with both copies.
    */
  def byId[A, I](id: A => I): Same[List[A]] = new Same[List[A]]:
    def diff(local: List[A], server: List[A]): Option[Diff] =
      val localById = local.groupBy(id)
      val serverById = server.groupBy(id)
      // One copy as it is; several (an id found twice), one line per copy
      def version(copies: Option[List[A]]): Option[String] =
        copies.map(_.mkString("\n"))
      val items =
        (localById.keySet ++ serverById.keySet).toList
          .filter(key => localById.get(key) != serverById.get(key))
          .map(key =>
            Diff.Item(
              key.toString,
              version(localById.get(key)),
              version(serverById.get(key))
            )
          )
          .sortBy(_.id)
      Option.when(items.nonEmpty)(Diff(items))
end Same
