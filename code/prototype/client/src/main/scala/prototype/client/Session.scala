package prototype.client

import com.raquo.laminar.api.L.{*, given}
import prototype.api.Token
import com.raquo.airstream.web.WebStorageVar

object Session:

  private val usernameStore: WebStorageVar[String] =
    WebStorageVar
      .sessionStorage(
        key = "username",
        syncOwner = None
      )
      .text(default = "")
  private val tokenStore: WebStorageVar[String] =
    WebStorageVar
      .sessionStorage(
        key = "token",
        syncOwner = None
      )
      .text(default = "")

  val currentUserS: Signal[Option[String]] =
    usernameStore.signal.combineWith(tokenStore.signal).map {
      case ("", _) | (_, "") => None
      case (username, _)     => Some(username)
    }

  val currentTokenS: Signal[Option[Token]] =
    tokenStore.signal.map {
      case ""    => None
      case value => Some(Token(value))
    }

  def login(username: String, token: Token): Unit =
    Var.set(usernameStore -> username, tokenStore -> token.value)

  def logout(): Unit =
    Var.set(usernameStore -> "", tokenStore -> "")

  def currentToken: Option[Token] =
    tokenStore.now() match
      case ""    => None
      case value => Some(Token(value))
