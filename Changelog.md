## 1.4.2 -- 2026-01-28

* Build with GHC 9.14

## 1.4.1 -- 2025-08-05

* Build with GHC 9.12

## 1.4.0 -- 2023-03-26

* Build with GHC 9.6
* Build with `free-5.2`
* Remove instances for `ListT`, as this type is deprecated and removed from `transformers-0.6`.
* Remove instances for `ErrorT`, as this type is deprecated and removed from `transformers-0.6`.

## 1.3.13 -- 2022-02-20

### Other changes

* Increased the upper-bound of time to < 1.13.
* Increased the upper-bound of base to < 4.17.
* Increased the upper-bound of semigroups to < 0.21.

## 1.3.12 -- 2020-12-14

### Other changes

* Increase the upper-bound of time to < 1.12.
* Increase the upper-bound of prettyprinter to < 1.8.

---

## 1.3.11 -- 2020-08-18

### Other changes

* Increase the lower-bound of unliftio-core to 0.2.0.0

---

## 1.3.10 -- 2020-06-17

### Other changes

* Add MonadUnliftIO instance to DiscardLoggingT
* Support `base-4.14`

---

## 1.3.9 -- 2020-01-25

### Other Changes

* Support `prettyprinter-1.6`

---

## 1.3.8 -- 2019-11-10

### Other Changes

* Support `prettyprinter-1.5`

---

## 1.3.7 -- 2019-10-22

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
