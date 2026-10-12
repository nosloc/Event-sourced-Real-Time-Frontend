package prototype.server

trait ErrorCode

final case class UserFacingError(
    code: ErrorCode,
    parameters: List[String] = List.empty,
    message: String = ""
) extends Throwable(
      s"ErrorCode: ${code.toString}, parameters: ${parameters.toString} ${
          if message.trim.isEmpty then "" else s"message: ${message.trim}"
        }"
    )

final case class InternalServerError(
    code: ErrorCode,
    parameters: List[String] = List.empty,
    message: String = ""
) extends Throwable(
      s"ErrorCode: ${code.toString}, parameters: ${parameters.toString} ${
          if message.trim.isEmpty then "" else s"message: ${message.trim}"
        }"
    )
