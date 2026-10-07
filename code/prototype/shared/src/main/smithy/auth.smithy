$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

// Services
@simpleRestJson
service AuthService {
    version: "1.0.0"
    operations: [
        Login
    ]
}

// Operations
@http(method: "POST", uri: "/api/login", code: 200)
operation Login {
    input: LoginInput
    output: LoginOutput
    errors: [
        InvalidCredentials
    ]
}

// Structures
structure LoginInput {
    @required
    username: String
}

structure LoginOutput {
    @required
    token: Token
}

@error("client")
@httpError(401)
structure InvalidCredentials {}
