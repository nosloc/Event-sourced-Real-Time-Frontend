$version: "2.0"

namespace prototype.api

// One event as the API gives it out, in a history page or on the live
// stream: its metadata, and the event itself as JSON
structure EventMessage {
    @required
    position: Long

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

list EventTags {
    member: String
}
