package prototype.server

import cats.effect.*
import cats.implicits.*
import java.util.UUID
import prototype.api.*
import scala.concurrent.duration.*

class AuthServiceImpl(
    loggedInUsers: Ref[IO, Map[Token, TherapistId]],
    therapistService: TherapistService[IO]
) extends AuthService[IO]:

  override def login(username: String): IO[LoginOutput] =
    val token =
      for
        _ <- IO.sleep(1.second) // Simulate a delay for the login process
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
