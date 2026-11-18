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
        GetHeadPosition
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

// The position of the newest event in the store, whatever its type: what a
// browser that receives every event should have reached once things settle
@readonly
@http(method: "GET", uri: "/api/events/head", code: 200)
operation GetHeadPosition {
    output: HeadPosition
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

structure HeadPosition {
    // 0 when the store holds no event
    @required
    position: Long
}

structure GetLatestEventsInput {
    @default(50)
    @httpQuery("take")
    take: Integer
}
