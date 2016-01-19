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
main = defaultMain [ bgroup "log10k" [ bench "logging-effect" (nfIO (LoggingEffect.runLoggingT (replicateM_ 1000 loggingEffectLog) loggingEffectStdoutHandler))
                                     , bench "monad-logger" (nfIO (MonadLogger.runLoggingT (replicateM_ 1000 monadLoggerLog) monadLoggerStdoutHandler))]
                   , bgroup "log10k-batched"
                            [ bench "logging-effect" (nfIO (LoggingEffect.withFDHandler LoggingEffect.defaultBatchingOptions stdout 0.4 80 $ \h ->
                                                            LoggingEffect.runLoggingT (replicateM_ 1000 loggingEffectLog)
                                                                                      (h . LoggingEffect.renderWithSeverity id)))
                                     , bench "monad-logger" (nfIO (MonadLogger.runStdoutLoggingT (replicateM_ 1000 monadLoggerLog)))]
                   , bgroup "log10k-batched-async"
                            [ bench "logging-effect" (nfIO (LoggingEffect.withFDHandler LoggingEffect.defaultBatchingOptions stdout 0.4 80 $ \h ->
                                                            LoggingEffect.runLoggingT (nThreads 10 (replicateM_ 100 loggingEffectLog))
                                                                                      (h . LoggingEffect.renderWithSeverity id)))
                                     , bench "monad-logger" (nfIO (MonadLogger.runStdoutLoggingT (nThreads 10 (replicateM_ 100 (MonadLogger.logDebugNS "?" "Log message")))))]]

loggingEffectStdoutHandler = PP.putDoc . (<> PP.linebreak) . LoggingEffect.renderWithSeverity id

loggingEffectLog = LoggingEffect.logMessage (LoggingEffect.WithSeverity LoggingEffect.Debug "Log message")

monadLoggerLog = MonadLogger.logDebugNS "?" "Log message"

monadLoggerStdoutHandler = \_ _ level str -> BS.putStrLn (fromLogStr (toLogStr (show level) <> str))

nThreads n m = runConcurrently (sequenceA_ (replicate n (Concurrently m)))
