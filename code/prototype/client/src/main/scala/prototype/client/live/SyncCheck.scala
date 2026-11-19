package prototype.client.live

import cats.effect.IO

import prototype.client.Api
import prototype.client.live.SyncCheckModel.{Input, Local}

import com.raquo.laminar.api.L.*

/** Runs the check of one source against the server (`SyncCheckModel.step`) on
  * each check of the stream, and the reads it asks for. Nothing runs until the
  * status is read: reading it also starts the source.
  */
object SyncCheck:

  // A read's answer, for the key it was asked for
  private final case class Answer[K, A](key: K, result: Either[Throwable, A])

  /** @param name
    *   the source's name, under which the status shows it
    * @param localS
    *   the source as the check sees it
    * @param load
    *   reads the server's value for a key
    */
  def apply[K, A](
      feed: LiveFeed,
      name: String,
      localS: Signal[Local[K, A]],
      load: K => IO[A],
      same: Same[A]
  ): Signal[SyncStatus] =
    // The answers of the reads, sent back into the model by `runReads`
    val answers: EventBus[Answer[K, A]] = new EventBus

    val inputs: EventStream[Input[K, A]] =
      EventStream.merge(
        feed.streamStatusS.changes
          .withCurrentValueOf(localS)
          .map((stream, local) => Input.Tick(stream, local)),
        answers.events
          .withCurrentValueOf(localS)
          .map((answer, local) => Input.Answered(answer.key, answer.result, local))
      )

    val stepS: Signal[(SyncCheckModel.State[K], Option[K])] =
      inputs.scanLeft((SyncCheckModel.initial[K], Option.empty[K]))(
        (acc, input) => SyncCheckModel.step(same)(acc._1, input)
      )

    // Runs each read the model asks for and sends its answer back. It never
    // emits: it is only there so that the reads run while the status is read
    // (the same way as `RefreshedLiveData.runLoads`). The model asks for one
    // read at a time, so a switch only drops a read it gave up on
    val runReads: Signal[Unit] =
      stepS.changes
        .collect { case (_, Some(key)) => key }
        .flatMapSwitch(key => Api.stream(load(key)).map(Answer(key, _)))
        .tapEach(answers.emit)
        .filter(_ => false)
        .mapToUnit
        .startWith(())

    stepS
      .combineWithFn(runReads)((step, _) => SyncStatus.of(name, step._1.check))
      .distinct
  end apply
end SyncCheck
