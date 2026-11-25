package prototype.shared

import prototype.api.Cursor

object Cursors:

  /** The event store's delivery order: by commit group, then by position. Must
    * match P4S's `Ordering[ReadCursor]`. Use with
    * `import prototype.shared.Cursors.given`.
    */
  given ordering: Ordering[Cursor] =
    Ordering.by(cursor => (cursor.commitGroup, cursor.position))
