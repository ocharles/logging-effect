{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM_)
import Criterion.Main
import qualified Control.Monad.Log as LoggingEffect
import qualified Control.Monad.Logger as MonadLogger
import qualified Data.ByteString as BS
import System.Log.FastLogger (fromLogStr, toLogStr)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint.Leijen.Text as PP
import System.IO (stdout)
import Control.Concurrent.Async.Lifted
import Data.Foldable (sequenceA_)

main :: IO ()
main = defaultMain [ bgroup "log10k" [ bench "logging-effect" (nfIO (LoggingEffect.runLoggingT loggingEffectLog loggingEffectStdoutHandler))
                                     , bench "monad-logger" (nfIO (MonadLogger.runLoggingT monadLoggerLog monadLoggerStdoutHandler))]
                   , bgroup "log10k-batched"
                            [ bench "logging-effect" (nfIO (LoggingEffect.withFDHandler LoggingEffect.defaultBatchingOptions stdout 0.4 80 $ \h ->
                                                            LoggingEffect.runLoggingT loggingEffectLog
                                                                                      (h . LoggingEffect.renderWithSeverity id)))
                                     , bench "monad-logger" (nfIO (MonadLogger.runStdoutLoggingT monadLoggerLog))]
                   , bgroup "log10k-batched-async"
                            [ bench "logging-effect" (nfIO (LoggingEffect.withFDHandler LoggingEffect.defaultBatchingOptions stdout 0.4 80 $ \h ->
                                                            LoggingEffect.runLoggingT (nThreads 10 loggingEffectLog)
                                                                                      (h . LoggingEffect.renderWithSeverity id)))
                                     , bench "monad-logger" (nfIO (MonadLogger.runStdoutLoggingT (nThreads 10 (MonadLogger.logDebugNS "?" "Log message"))))]
                   , bgroup "map-and-log" [ bench "map-once" (nfIO (LoggingEffect.runLoggingT (LoggingEffect.mapLogMessage id $ LoggingEffect.mapLogMessage id $ LoggingEffect.mapLogMessage id $ LoggingEffect.mapLogMessage id loggingEffectLog) loggingEffectStdoutHandler))]
                   ]

loggingEffectStdoutHandler = PP.putDoc . (<> PP.linebreak) . LoggingEffect.renderWithSeverity id

loggingEffectLog :: LoggingEffect.MonadLog (LoggingEffect.WithSeverity PP.Doc) m => m ()
loggingEffectLog = LoggingEffect.logMessage (LoggingEffect.WithSeverity LoggingEffect.Debug "Log message")

monadLoggerLog = MonadLogger.logDebugNS "?" "Log message"

monadLoggerStdoutHandler = \_ _ level str -> BS.putStrLn (fromLogStr (toLogStr (show level) <> str))

nThreads n m = runConcurrently (sequenceA_ (replicate n (Concurrently m)))
