package prototype.client.live

import cats.effect.IO

import prototype.api.EventMessage
import prototype.client.live.LiveFeed.FeedMessage

import com.raquo.laminar.api.L.*

/** A `LiveDataSource` for a value that does not depend on a key, such as a list
  * shown whole (the companies of the admin tab). It is a keyed source with a
  * constant key, so it loads, replays and reloads exactly the same way; only
  * the key is gone from `load` and `evolve`.
  */
final class KeylessLiveDataSource[A, E](
    feed: EventBus[FeedMessage],
    load: IO[(A, Long)],
    decode: EventMessage => Option[E],
    evolve: (A, E) => A
):

  private val source: LiveDataSource[Unit, A, E] =
    new LiveDataSource[Unit, A, E](
      feed = feed,
      key = Signal.fromValue(()),
      load = _ => load,
      decode = decode,
      evolve = (_, value, event) => evolve(value, event)
    )

  val valuesS: Signal[Option[A]] = source.valuesS
  val loadingS: Signal[Boolean] = source.loadingS
  val errorS: Signal[Option[Throwable]] = source.errorS

  /** See `LiveDataSource.keepLoaded`. */
  def keepLoaded: Mod[HtmlElement] = source.keepLoaded
