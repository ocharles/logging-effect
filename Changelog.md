## Unreleased

### Other Changes

* Support `prettyprinter-1.4`

---

## 1.3.6 -- 2019-09-19

### New

* Added `MonadFail` instances.

---

## 1.3.5 -- 2019-09-17

### Other Changes

* Support `base-4.13`
* Support GHC 8.8.1
* Support `prettyprinter-1.3`

---

## 1.3.4 -- 2019-05-15

### Other Changes

* Support `semigroups-0.19`.

---

## 1.3.3 -- 2018-09-30

### Other Changes

* Support `stm-2.5`.
* Support `base-4.12`.

---

## 1.3.2 -- 2018-07-06

### Other Changes

* Support `free-5.1`.

---

## 1.3.1

* Add `MonadUnliftIO` instance for `LoggingT`.

---

# 1.3.0

## Major Changes

* Switch from `wl-pprint-text` to `prettyprinter`.

## Other changes

* Change the type of the `ribbonFrac` parameter of `withFDHandler`
  from `Float` to `Double` to reflect the underlying `prettyprinter`
  API.

---

# 1.2.6

## Other Changes

* Increased upper bound of `base` and support GHC 8.4.

---

# 1.2.5

## Other Changes

* Increased upper bound of `exceptions`.

---

# 1.2.4

## Other Changes

* Increased upper bound of `exceptions`.

---

# 1.2.3

## Other Changes

* Increased upper of `async`.

---

# 1.2.2

## Other Changes

* Increased upper bound of `free` and `time`.

---

# 1.2.1

## Other Changes

* Increased upper bound of `base` to allow < 4.11.

---

# 1.2.0

## Major Changes

- `withFDHandler` now explicitly flushes the file handle whenever log entries
   are rendered out. Thanks to @filterfish for identifying this omission that
   could lead to log messages being dropped.

   Upgrade steps: no changes other than updating `logging-effect`.

---

# 1.1.3

## Other Changes

- Increased upper bound of `time` to allow < 1.9.

---

# 1.1.2

## Other changes

- Increased upper bound of `time`

---

# 1.1.1

- `withBatchedHandler` no longer prints empty log messages. Previously,
  if you ran a program that didn't log but used `withBatchedHandler` (or anything
  that used that), an empty log message would be output. Thanks to @codedmart
  for fixing this.

---

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

- INLINEABLE pragmas added.

---

# 1.0.0

- Initial release
