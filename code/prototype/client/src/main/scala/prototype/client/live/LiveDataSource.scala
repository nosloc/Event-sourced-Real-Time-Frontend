package prototype.client.live

import com.raquo.laminar.api.L.*
import cats.effect.IO
import prototype.client.Api.ApiCall
import prototype.client.Api

class LiveDataSource[A, E, D](
    val eventBus: EventBus[String],
    val opened: EventBus[Unit],
    val dependencies: Signal[D],
    val fetchIO: D => IO[(A, Long)],
    val decode: String => Option[E],
    val collect: (D, A, E) => Option[A]
):
  val valuesVar: Var[Option[A]] = Var(None)
  val errorVar: Var[Option[Throwable]] = Var(None)
  val loadingVar: Var[Boolean] = Var(false)
  val globalPositionVar: Var[Option[Long]] = Var(None)

  val valuesS: Signal[Option[A]] = valuesVar.signal
  val errorS: Signal[Option[Throwable]] = errorVar.signal
  val loadingS: Signal[Boolean] = loadingVar.signal
  val globalPositionS: Signal[Option[Long]] = globalPositionVar.signal

  val stateS: Signal[Api.ApiState[A]] = Signal
    .combine(
      valuesS,
      errorS,
      loadingS
    )
    .map {
      case (Some(value), _, _) => Api.Success(value)
      case (_, Some(error), _) => Api.Error(error)
      case (_, _, true)        => Api.Loading
      case _                   => Api.Loading
    }

  def bind(
  ): Mod[HtmlElement] =
    Seq(
      opened.events.withCurrentValueOf(dependencies).flatMapSwitch {
        (dependencies) =>
          ApiCall.fromIOCall(fetchIO(dependencies), identity).stateS.changes
      } --> {
        case Api.Success((value, position)) =>
          errorVar.set(None)
          valuesVar.set(Some(value))
          globalPositionVar.set(Some(position))
        case Api.Error(error) =>
          errorVar.set(Some(error))
        case Api.Loading =>
          loadingVar.set(true)
      },
      dependencies.changes.flatMapSwitch((dependencies) =>
        ApiCall.fromIOCall(fetchIO(dependencies), identity).stateS.changes
      ) --> {
        case Api.Success((value, position)) =>
          errorVar.set(None)
          valuesVar.set(Some(value))
          globalPositionVar.set(Some(position))
        case Api.Error(error) =>
          errorVar.set(Some(error))
        case Api.Loading =>
          loadingVar.set(true)
      },
      eventBus.events.withCurrentValueOf(dependencies) --> { (raw, dep) =>
        decode(raw).foreach(event =>
          valuesVar.update {
            case Some(value) =>
              collect(dep, value, event) match
                case Some(newValue) => Some(newValue)
                case None           => Some(value)
            case None => None
          }
        )

      }
    )
