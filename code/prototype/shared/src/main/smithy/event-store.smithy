$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

@simpleRestJson
@httpBearerAuth
service EventStoreService {
    version: "1.0.0"
    operations: [
        ListEvents
        GetLatestEvents
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/events", code: 200)
operation ListEvents {
    input: ListEventsInput
    output: EventsPage
}

@readonly
@http(method: "GET", uri: "/api/events/latest", code: 200)
operation GetLatestEvents {
    input: GetLatestEventsInput
    output: EventsPage
}

// Structures
structure ListEventsInput {
    @default(0)
    @httpQuery("skip")
    skip: Long

    @default(50)
    @httpQuery("take")
    take: Integer
}

structure EventsPage {
    @required
    events: EventMessages

    @required
    total: Long
}

structure GetLatestEventsInput {
    @default(50)
    @httpQuery("take")
    take: Integer
}
