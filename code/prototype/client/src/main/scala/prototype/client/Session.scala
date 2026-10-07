package prototype.client

import com.raquo.laminar.api.L.{*, given}

object Session:

  private val currentUser: Var[Option[String]] = Var(None)
  val currentUserS: Signal[Option[String]] = currentUser.signal

  def login(username: String): Unit =
    currentUser.set(Some(username))

  def logout(): Unit =
    currentUser.set(None)
