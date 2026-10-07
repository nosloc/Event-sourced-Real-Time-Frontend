$version: "2.0"

namespace prototype.api

@error("client")
@httpError(401)
structure Unauthorized {}

string Token
