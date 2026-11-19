package prototype.client.live

/** What differs between the browser's value and the server's, by item id for a
  * list (`Same.byId`), or the whole value otherwise (`Same.equal`).
  */
final case class Diff(
    onlyLocal: List[String],
    onlyServer: List[String],
    different: List[String]
):
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

  /** A value compared whole: it differs, with no detail. */
  val wholeValue: Diff = Diff(Nil, Nil, List("the whole value"))

/** How a live value is compared with the server's: None when they match,
  * otherwise what differs.
  */
trait Same[A]:
  def diff(local: A, server: A): Option[Diff]

object Same:

  /** Compared whole, with `==`. For a list, the order counts. */
  def equal[A]: Same[A] = new Same[A]:
    def diff(local: A, server: A): Option[Diff] =
      Option.when(local != server)(Diff.wholeValue)

  /** A list compared item by item, by `id`: the order of the list does not
    * count, the items' values do. An id found twice on one side (an event
    * applied twice) is reported as different.
    */
  def byId[A, I](id: A => I): Same[List[A]] = new Same[List[A]]:
    def diff(local: List[A], server: List[A]): Option[Diff] =
      val localById = local.groupBy(id)
      val serverById = server.groupBy(id)
      def ids(keys: Iterable[I]): List[String] = keys.map(_.toString).toList.sorted
      val result = Diff(
        onlyLocal = ids(localById.keySet -- serverById.keySet),
        onlyServer = ids(serverById.keySet -- localById.keySet),
        different = ids(
          localById.keySet
            .intersect(serverById.keySet)
            .filter(key => localById(key) != serverById(key))
        )
      )
      Option.when(
        result.onlyLocal.nonEmpty || result.onlyServer.nonEmpty ||
          result.different.nonEmpty
      )(result)
end Same
