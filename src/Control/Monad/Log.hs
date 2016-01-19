{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|

'MonadLog' is a general purpose logging effect. Unlike other logging
libraries available on Hackage, 'MonadLog' does /not/ assume that you
will be logging text information. Instead, the choice of logging data
is up to you. This, combined with 'mapLogMessage' and the ability to
/reintepret/ log messages results in an extremely powerful set of
logging combinators.

This library also comes with a flexible default implementation of
'MonadLog'. 

We'll explore this power in the tutorial.

/Tutorial/:

To add logging to your applications, you will need to make two changes.

First, use the 'MonadLog' type class to indicate that a computation has
access to logging. 'MonadLog' is parameterized on the type of messages
that you intend to log. In this example, we will log 'Text' that is
wrapped in the 'WithSeverity'. Note that this does /not/ specify how
to perform the actual logging.

@
testApp :: MonadLog (WithSeverity Text) m => m ()
testApp = do
  logMessage (WithSeverity Info "Don't mind me")
  logMessage (WithSeverity Error "But do mind me!")
@

Next, we need to run this computation under a 'MonadLog' effect handler.
In this example, we open two file descriptor loggers, so we can dispatch
our log events to @STDOUT@ and @STDERR@. We use 'choose' to split out log
messages between these two 'Handler's - messages with a 'msgSeverity' of
'Error' will go to @STDERR@, and all other messages will go to @STDOUT@.

Note that both our @STDOUT@ and @STDERR@ handlers only know how to log
'Text' (see the type of 'withFDHandler'). However, we're logging
@WithSeverity@ @Text@, so we need to adjust these handles. To do so, we
use function pre-composition to adjust their inputs. For @stdoutHandler@, I use
'renderWithSeverity' to render the severity information, and then pass the
log message through unchanged. For @stderrHandler@ I also render the
severity information, but I also transform the text into all caps -
I really don't want to miss those messages!

@
withSplitLogging :: 'LoggingT' ('WithSeverity' 'Text') 'IO' () -> 'IO' ()
withSplitLogging m =
  'withFDHandler' 'defaultBatchingOptions' 'stderr' $ \stderrHandler ->
  'withFDHandler' 'defaultBatchingOptions' 'stdout' $ \stdoutHandler ->
  'runLoggingT' m 
              (\\message ->
                 case 'msgSeverity' message of
                   'Error' -> stderrHandler ('renderWithSeverity' 'id' message)
                   _ ->
                     stdoutHandler
                       ('renderWithSeverity' ("T".'T.map' 'toUpper')
                                           message))
@

-}

module Control.Monad.Log
       ( -- * @MonadLog@
         MonadLog(..), mapLogMessage,

         -- * Message wrappers
         -- ** Timestamps
         WithTimestamp(..), withTimestamps, renderWithTimestamp,
         -- ** Severity
         WithSeverity(..), Severity(..), renderWithSeverity,

         -- * @LoggingT@, a general handler
         LoggingT, pattern LoggingT, runLoggingT, mapLoggingT,

         -- ** 'LoggingT' Handlers
         Handler, withFDHandler,

         -- *** Batched handlers
         withBatchedHandler, BatchingOptions(..), defaultBatchingOptions,

         -- * Pure logging
         PureLoggingT(..), runPureLoggingT,

         -- * Discarding logs
         DiscardLoggingT, discardLogging
       ) where

import Data.Coerce
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay
import Control.Applicative
import Control.Monad (MonadPlus, guard)
import Control.Monad.Catch
       (MonadThrow(..), MonadMask(..), MonadCatch(..), bracket)
import Control.Monad.Cont.Class (MonadCont(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Fix
import Control.Monad.Free.Class (MonadFree(..))
import Control.Monad.RWS.Class (MonadRWS(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.Chan
       (newChan, writeChan, getChanContents)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Char
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.IO (hPutStr)
import Data.Time (UTCTime, TimeLocale, getCurrentTime)
import System.IO (Handle, stderr, stdout)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | The class of monads that support logging.
class Monad m => MonadLog message m | m -> message where
  -- | Append a message to the log for this computation.
  logMessage :: message -> m ()

-- | Re-interpret the log messages in one computation. This can be useful to
-- embed a computation with one log type in a larger general computation.
mapLogMessage
  :: MonadLog message' m
  => (message -> message') -> LoggingT message m a -> m a
mapLogMessage f m =
  runLoggingT m
              (logMessage . f)

--------------------------------------------------------------------------------
-- | Add \"Severity\" information to a log message. This is often used to convey
-- how significant a log message is.
data WithSeverity a =
  WithSeverity {msgSeverity :: Severity -- ^ Retrieve the 'Severity' a message.
               ,discardSeverity :: a -- ^ View the underlying message.
               }
  deriving (Eq,Ord,Read,Show,Functor)

-- | Classes of severity for log messages. These have been chosen to match
-- @syslog@ severity levels
data Severity =
 Emergency -- ^ System is unusable. By @syslog@ convention, this level should not be used by applications.
 | Alert -- ^ Should be corrected immediately.
 | Critical -- ^ Critical conditions.
 | Error -- ^ Error conditions.
 | Warning -- ^ May indicate that an error will occur if action is not taken.
 | Notice -- ^ Events that are unusual, but not error conditions.
 | Informational -- ^ Normal operational messages that require no action.
 | Debug -- ^ Information useful to developers for debugging the application.
  deriving (Eq,Enum,Bounded,Read,Show,Ord)

-- | Given a way to render the underlying message @a@ render a message with its
-- timestamp.
--
-- >>> renderWithSeverity id Debug (WithSeverity Info "Flux capacitor is functional")
-- [Info] Flux capacitor is functional
renderWithSeverity
  :: (a -> Text) -> (WithSeverity a -> Text)
renderWithSeverity k (WithSeverity u a) = "[" <> pack (show u) <> "] " <> k a

--------------------------------------------------------------------------------
-- | Add a timestamp to log messages.
data WithTimestamp a =
  WithTimestamp {discardTimestamp :: a -- ^ Retireve the time a message was logged.
                ,msgTimestamp :: UTCTime -- ^ View the underlying message.
                }

-- | Given a way to render the underlying message @a@ and a way to format
-- 'UTCTime', render a message with its timestamp.
--
-- >>> renderWithTimestamp (formatTime defaultTimeLocale rfc822DateFormat) id timestamppedLogMessage
-- [Tue, 19 Jan 2016 11:29:42 UTC] Setting target speed to plaid
renderWithTimestamp :: (UTCTime -> String)
                       -- ^ How to format the timestamp. 
                    -> (a -> Text)
                       -- ^ How to render the rest of the message.
                    -> (WithTimestamp a -> Text)
renderWithTimestamp formatter k (WithTimestamp a t) =
  "[" <> pack (formatter t) <> "] " <> k a

-- TODO Is this faster with a custom handler?
-- logMessage msg = liftIO getCurrentTime >>= \t -> lift (logMessage (WithTimestamp t msg))
-- | Add timestamps to all messages logged. Timestamps will be calculated
-- synchronously when log entries are emitted.
withTimestamps :: (MonadLog (WithTimestamp message) m,MonadIO m)
               => LoggingT message m a -> m a
withTimestamps m =
  runLoggingT
    m
    (\msg ->
       do now <- liftIO getCurrentTime
          logMessage (WithTimestamp msg now))

--------------------------------------------------------------------------------
-- | 'LoggingT' is a very general handler for the 'MonadLog' effect. Whenever a
-- log entry is emitted, the given 'Handler' is invoked, producing some
-- side-effect (such as writing to @stdout@, or appending a database table).
newtype LoggingT message m a =
  MkLoggingT {unLoggingT :: ReaderT (Handler m message) m a}
  deriving (Monad,Applicative,Functor,MonadFix,Alternative,MonadPlus,MonadIO,MonadWriter w,MonadCont,MonadError e,MonadMask,MonadCatch,MonadThrow,MonadState s)

-- | 'LoggingT' @messasge@ @m@ is isomorphic to @Handler message m -> m ()@.
-- This is a reader monad with base effects in @m@ and access to
-- @Handler message m@.This pattern synonym witnesses that isomorphism.
pattern LoggingT f = MkLoggingT (ReaderT f)

-- | Given a 'Handler' for a given @message@, interleave this 'Handler' into the
-- underlying @m@ computation whenever 'logMessage' is called.
runLoggingT
  :: LoggingT message m a -> Handler m message -> m a
runLoggingT (LoggingT m) handler = m handler

instance MonadTrans (LoggingT message) where
  lift = LoggingT . const

instance MonadReader r m => MonadReader r (LoggingT message m) where
  ask = lift ask
  local f (LoggingT m) = LoggingT (local f . m)
  reader f = lift (reader f)

-- | The main instance of 'MonadLog', which dispatches 'logMessage' calls to a 'Handler'.
instance Monad m => MonadLog message (LoggingT message m) where
  logMessage m = LoggingT (\f -> f m)

instance MonadRWS r w s m => MonadRWS r w s (LoggingT message m)

instance (Functor f,MonadFree f m) => MonadFree f (LoggingT message m)

-- | 'LoggingT' unfortunately does admit an instance of the @MFunctor@ type
-- class, which provides the @hoist@ method to change the monad underneith
-- a monad transformer. However, it is possible to do this with 'LoggingT'
-- provided that you have a way to re-interpret a log handler in the
-- original monad.
mapLoggingT :: (Handler n message' -> Handler m message)
               -- ^ Given a handler for the new monad @n@, embed this handler
               -- inside the original monad @m@.
            -> (forall a. m a -> n a)
               -- ^ A natural transformation/monad morphism from the original
               -- monad @m@ to the new monad @n@. For example, you could use
               -- 'liftIO' or 'runWriterT'.
            -> LoggingT message m a
            -> LoggingT message' n a
mapLoggingT etaHandler eta (LoggingT f) = LoggingT (eta . f . etaHandler)

--------------------------------------------------------------------------------
-- | Handlers are mechanisms to interpret the meaning of logging as an action
-- in the underlying monad. They are simply functions from log messages to
-- @m@-actions.
type Handler m message = message -> m ()

-- | Options that be used to configure 'withBatchingHandler'.
data BatchingOptions =
  BatchingOptions {flushMaxDelay :: Int -- ^ The maximum amount of time to wait between flushes
                  ,flushMaxQueueSize :: Int -- ^ The maximum amount of messages to hold in memory between flushes}
                  }
  deriving (Eq,Ord,Read,Show)

-- | Defaults for 'BatchingOptions'
--
-- @
-- 'defaultBatchingOptions' = 'BatchingOptions' { 'flushMaxDelay' = 1000000, 'flushMaxQueueSize' = 100 }
-- @
defaultBatchingOptions :: BatchingOptions
defaultBatchingOptions = BatchingOptions 1000000 100

-- | Create a new batched handler. Batched handlers take batches of messages to
-- log at once, which can be more performant than logging each individual
-- message.
--
-- A batched handler flushes under three criteria:
--
--   1. The flush interval has elapsed and the queue is not empty.
--   2. The queue has become full and needs to be flushed.
--   3. The scope of 'withBatchedHandler' is exited.
--
-- Batched handlers queue size and flush period can be configured via
-- 'BatchingOptions'.
withBatchedHandler :: (MonadIO io,MonadMask io)
                   => BatchingOptions
                   -> ([message] -> IO ())
                   -> (Handler io message -> io a)
                   -> io a
withBatchedHandler BatchingOptions{..} flush k =
  do do closed <- liftIO (newTVarIO False)
        channel <- liftIO (newTBQueueIO flushMaxQueueSize)
        bracket (liftIO (async (repeatWhileTrue (publish closed channel))))
                (\publisher ->
                   do liftIO (do atomically (writeTVar closed True)
                                 wait publisher))
                (\_ -> k (liftIO . atomically . writeTBQueue channel))
  where repeatWhileTrue m =
          do again <- m
             if again
                then repeatWhileTrue m
                else return ()
        publish closed channel =
          do flushAlarm <- newDelay flushMaxDelay
             (messages,stillOpen) <-
               atomically
                 (do messages <-
                       flushAfter flushAlarm <|> flushFull <|> flushOnClose
                     stillOpen <- fmap not (readTVar closed)
                     return (messages,stillOpen))
             flush messages
             pure stillOpen
          where flushAfter flushAlarm =
                  do waitDelay flushAlarm
                     isEmptyTBQueue channel >>= guard . not
                     emptyTBQueue channel
                flushFull =
                  do isFullTBQueue channel >>= guard
                     emptyTBQueue channel
                flushOnClose =
                  do readTVar closed >>= guard
                     emptyTBQueue channel
        emptyTBQueue q =
          do mx <- tryReadTBQueue q
             case mx of
               Nothing -> return []
               Just x -> fmap (x :) (emptyTBQueue q)
  
-- | 'withFDHandler' creates a new 'Handler' that will append a given file
-- descriptor (or 'Handle', as it is known in the "base" library). Note that
-- this 'Handler' requires log messages to be of type 'Text'. 
--
-- These 'Handler's asynchronously log messages to the given file descriptor,
-- rather than blocking.
withFDHandler
  :: (MonadIO io,MonadMask io)
  => BatchingOptions -> Handle -> (Handler io Text -> io a) -> io a
withFDHandler options fd =
  withBatchedHandler options
                     (hPutStr fd . T.unlines)

--------------------------------------------------------------------------------
-- | A 'MonadLog' handler optimised for pure usage. Log messages are accumulated
-- strictly, given that messasges form a 'Monoid'.
newtype PureLoggingT log m a = MkPureLoggingT (StateT log m a)
  deriving (Functor,Applicative,Monad,MonadFix,MonadCatch,MonadThrow,MonadIO,MonadMask,MonadReader r,MonadWriter w,MonadCont,MonadError e,Alternative,MonadPlus)

runPureLoggingT
  :: Monoid log
  => PureLoggingT log m a -> m (a,log)
runPureLoggingT (MkPureLoggingT (StateT m)) = m mempty

mkPureLoggingT
  :: (Monad m,Monoid log)
  => m (a,log) -> PureLoggingT log m a
mkPureLoggingT m =
  MkPureLoggingT
    (StateT (\s ->
               do (a,l) <- m
                  return (a,s <> l)))

instance MonadTrans (PureLoggingT log) where
  lift = MkPureLoggingT . lift

instance (Functor f, MonadFree f m) => MonadFree f (PureLoggingT log m)

-- | A pure handler of 'MonadLog' that accumulates log messages under the structure of their 'Monoid' instance.
instance (Monad m, Monoid log) => MonadLog log (PureLoggingT log m) where
  logMessage message = mkPureLoggingT (return ((), message)) 

instance MonadRWS r w s m => MonadRWS r w s (PureLoggingT message m)

instance MonadState s m => MonadState s (PureLoggingT log m) where
  state f = lift (state f) 
  get = lift get
  put = lift . put 

--------------------------------------------------------------------------------
newtype DiscardLoggingT message m a =
  DiscardLoggingT {discardLogging :: m a}
  deriving (Functor,Applicative,Monad,MonadFix,MonadCatch,MonadThrow,MonadIO,MonadMask,MonadReader r,MonadWriter w,MonadCont,MonadError e,Alternative,MonadPlus,MonadState s,MonadRWS r w s)

instance MonadTrans (DiscardLoggingT message) where
  lift = DiscardLoggingT

instance (Functor f,MonadFree f m) => MonadFree f (DiscardLoggingT message m)

-- | The trivial instance of 'MonadLog' that simply discards all messages logged.
instance Monad m => MonadLog message (DiscardLoggingT message m) where
  logMessage _ = return ()

--------------------------------------------------------------------------------
-- Test cases
testApp :: MonadLog (WithSeverity Text) m
        => m ()
testApp =
  do logMessage (WithSeverity Informational "Don't mind me")
     logMessage (WithSeverity Error "But do mind me!")

withSplitLogging
  :: LoggingT (WithSeverity Text) IO () -> IO ()
withSplitLogging m =
  withFDHandler defaultBatchingOptions stderr $
  \stderrHandler ->
    withFDHandler defaultBatchingOptions stdout $
    \stdoutHandler ->
      runLoggingT
        m
        (\message ->
           case msgSeverity message of
             Error -> stdoutHandler (renderWithSeverity id message)
             _ ->
               stdoutHandler
                 (renderWithSeverity (T.map toUpper)
                                     message))
