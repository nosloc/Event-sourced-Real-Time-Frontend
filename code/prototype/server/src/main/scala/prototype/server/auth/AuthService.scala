package prototype.server.auth

import cats.effect.*
import prototype.api.*

class AuthServiceImpl(
    loggedInUsers: Ref[IO, Map[Token, TherapistId]],
    therapistService: TherapistService[IO]
) extends AuthService[IO]:

  override def login(username: String): IO[LoginOutput] =
    val token =
      for
        therapist <- therapistService
          .getTherapist(TherapistId(username))
          .map(_.therapist)
        uuid <- IO.randomUUID
        token = Token(uuid.toString)
        _ <- loggedInUsers.update(_ + (token -> therapist.therapistId))
      yield token

    token
      .map(LoginOutput(_))
      .adaptError { case TherapistNotFound() =>
        InvalidCredentials()
      }
  end login
end AuthServiceImpl
