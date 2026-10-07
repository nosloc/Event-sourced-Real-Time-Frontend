package prototype.server.auth

import cats.effect.*
import prototype.api.*

trait AuthChecker:
  def userFor(token: String): IO[Option[TherapistId]]
end AuthChecker

object AuthChecker:
  def apply(loggedInUsers: Ref[IO, Map[Token, TherapistId]]): AuthChecker =
    new AuthChecker:
      override def userFor(token: String): IO[Option[TherapistId]] =
        loggedInUsers.get.map(_.get(Token(token)))
