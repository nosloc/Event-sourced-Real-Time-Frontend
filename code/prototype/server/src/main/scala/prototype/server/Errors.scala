package prototype.server

trait UserFacingFailure extends Throwable with Product:
  def message: String
  def errorCode: String = productPrefix
  def parameters: List[String] = Nil
