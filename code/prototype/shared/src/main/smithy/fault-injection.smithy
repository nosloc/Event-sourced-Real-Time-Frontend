$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

// Faults injected on purpose, to make the problems of the live views happen
// on the running prototype. Held in memory: a restart clears them. Mounted
// only when the server runs with faults enabled (FAULTS_ENABLED=true).
@simpleRestJson
@httpBearerAuth
service FaultInjectionService {
    version: "1.0.0"
    operations: [
        GetFaults
        SetFaults
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/faults", code: 200)
operation GetFaults {
    output: Faults
}

// Replaces every fault: what the input leaves out is turned off
@idempotent
@http(method: "PUT", uri: "/api/faults", code: 200)
operation SetFaults {
    input: SetFaultsInput
    output: Faults
}

// Structures
structure SetFaultsInput {
    @required
    @httpPayload
    faults: Faults
}

structure Faults {
    // Added after a snapshot was read, before it is sent
    @required
    snapshotDelayMillis: DelayMillis

    // A read model left out has no delay; one listed twice keeps its last
    // entry
    @required
    readModelDelays: ReadModelDelays
}

list ReadModelDelays {
    member: ReadModelDelay
}

// Added before the read model applies each event
structure ReadModelDelay {
    @required
    readModel: ReadModel

    @required
    delayMillis: DelayMillis
}

// The read models that can be slowed down, one per projection
enum ReadModel {
    COMPANIES
    THERAPISTS
    PATIENTS
    APPOINTMENTS
}

// 0 means no delay
@range(min: 0, max: 60000)
integer DelayMillis
