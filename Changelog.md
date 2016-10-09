# 1.1.0

*Breaking changes*:

- `MonadLog` no longer has `logMessage` as a function. It now has
  `logMessageFree` which takes a free monoid of log messages. If you were just
  using `logging-effect` then this won't affect you, as `logMessage` still exists
  with the same signature outside the type class.

- `MonadLog` now comes with a law that states that logging is a monoid
  homomorphism. This essentially means that you have to treat all log messages
  uniformly.

- Pass-through instances for all "stock" monad transformers have been added
  (all of `transformers`, `CatchT` from exceptions and `FreeT`/`FT` from `free`).

- `WithSeverity` now has instances of `Traversable` and `Foldable`

- `WithTimestamp` now has instances of `Eq`, `Ord`, `Read` and `Show`.

*Additions*:

- A set of convenience functions have been added for quickly logging with
  severity. The combinators are: `logDebug`, `logInfo`, `logNotice`,
  `logWarning`, `logError`, `logCritical`, `logAlert` and `logEmergency`.

- `mapLogMessage` got a companion function `mapLogMessageM` that works with 
  monadic tranformations.

*Other*

- Many documentation bug fixes.


# 1.0.0

- Initial release
