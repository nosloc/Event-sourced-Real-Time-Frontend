package prototype.server.eventfeed

import prototype.api.Cursor

import persistent4s.ReadCursor

object ApiCursor:

  /** P4S's cursor as the API gives it out: the same pair, so the same order */
  extension (cursor: ReadCursor)
    def toApi: Cursor = Cursor(cursor.commitGroup, cursor.globalPosition)
