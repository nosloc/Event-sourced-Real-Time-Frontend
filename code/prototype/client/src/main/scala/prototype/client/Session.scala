package prototype.client

import com.raquo.laminar.api.L.{*, given}
import prototype.api.Token

case class SessionUser(username: String, token: Token)

object Session:

  private val currentUser: Var[Option[SessionUser]] = Var(None)
  val currentUserS: Signal[Option[String]] =
    currentUser.signal.map(_.map(_.username))

  def login(username: String, token: Token): Unit =
    currentUser.set(Some(SessionUser(username, token)))

  def logout(): Unit =
    currentUser.set(None)
