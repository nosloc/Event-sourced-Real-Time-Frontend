package prototype.server.eventfeed

import cats.effect.IO

import prototype.api.Cursor

import persistent4s.{EventEnvelope, ReadCursor}

object ApiCursor:

  /** P4S's cursor as the API gives it out: the same pair, so the same order */
  extension (cursor: ReadCursor)
    def toApi: Cursor = Cursor(cursor.commitGroup, cursor.globalPosition)

  /** The cursor of the last event a command wrote (its events share one commit
    * group), or None when it wrote none
    */
  def lastWritten(events: List[EventEnvelope[?]]): Option[Cursor] =
    events.map(_.metadata.cursor).maxOption.map(_.toApi)

  /** The same, for a command that always writes: fails when it wrote none */
  def required(cursor: Option[Cursor]): IO[Cursor] =
    IO.fromOption(cursor)(IllegalStateException("The command wrote no event"))
