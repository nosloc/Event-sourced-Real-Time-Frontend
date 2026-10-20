package prototype.server

trait ErrorCode

trait UserFacingFailure extends Throwable with Product:
  def message: String
  def errorCode: String = productPrefix
  def parameters: List[String] = Nil

final case class UserFacingError(
    code: ErrorCode,
    override val parameters: List[String] = List.empty,
    message: String = ""
) extends Throwable(
      s"ErrorCode: ${code.toString}, parameters: ${parameters.toString} ${
          if message.trim.isEmpty then "" else s"message: ${message.trim}"
        }"
    )
    with UserFacingFailure:
  override def errorCode: String = code.toString

final case class InternalServerError(
    code: ErrorCode,
    parameters: List[String] = List.empty,
    message: String = ""
) extends Throwable(
      s"ErrorCode: ${code.toString}, parameters: ${parameters.toString} ${
          if message.trim.isEmpty then "" else s"message: ${message.trim}"
        }"
    )
