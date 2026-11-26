$version: "2.0"

namespace prototype.api

// A point in the order the event store delivers events in: by commit group,
// then by position (P4S's `ReadCursor`). Compare with `Cursors.ordering`, never
// by position alone: a late commit can carry a lower position
structure Cursor {
    @required
    commitGroup: Long

    @required
    position: Long
}

// One event as the API gives it out, in a history page or on the live
// stream: its metadata, and the event itself as JSON
structure EventMessage {
    @required
    cursor: Cursor

    @required
    eventId: String

    @required
    eventType: String

    @required
    recordedAt: Timestamp

    @required
    tags: EventTags

    @required
    payload: String
}

list EventMessages {
    member: EventMessage
}

// What a list adds to its snapshot: the events of the read model's types after
// the snapshot's cursor that were deliverable when the list was read, and the
// cursor the answer is complete up to
structure CatchUp {
    // In delivery order
    @required
    events: EventMessages

    // The last of `events`, or the snapshot's cursor when there are none
    @required
    head: Cursor
}

list EventTags {
    member: String
}
