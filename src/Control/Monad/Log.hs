{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

{-|

'MonadLog' is a general purpose logging effect. Unlike other logging
libraries available on Hackage, 'MonadLog' does /not/ assume that you
will be logging text information. Instead, the choice of logging data
is up to you. This, combined with 'mapLogMessage' and the ability to
/reintepret/ log messages results in an extremely powerful set of
logging combinators.

This library also comes with a flexible default implementation of
'MonadLog'. By using 'Handler's along with their 'Contravariant',
'Decidable' and 'Divisible' instances, you are able to route log
messages to multiple sources.

We'll explore this power in the tutorial.

/Tutorial/:

To add logging to your applications, you will need to make two changes.

First, use the 'MonadLog' type class to indicate that a computation has
access to logging. 'MonadLog' is parameterized on the type of messages
that you intend to log. In this example, we will log 'Text' that is
wrapped in the 'WithUrgency'. Note that this does /not/ specify how
to perform the actual logging.

@
testApp :: MonadLog (WithUrgency Text) m => m ()
testApp = do
  logMessage (WithUrgency Info "Don't mind me")
  logMessage (WithUrgency Error "But do mind me!")
@

Next, we need to run this computation under a 'MonadLog' effect handler.
In this example, we open two file descriptor loggers, so we can dispatch
our log events to @STDOUT@ and @STDERR@. We use 'choose' to split out log
messages between these two 'Handler's - messages with a 'msgUrgency' of
'Error' will go to @STDERR@, and all other messages will go to @STDOUT@.

Note that both our @STDOUT@ and @STDERR@ handlers only know how to log
'Text' (see the type of 'withFDHandler'). However, we're logging
@WithUrgency@ @Text@, so we need to adjust these handles. To do so, we
use 'contramap' to adjust their inputs. For @stdoutHandler@, I use
'renderWithUrgency' to render the urgency information, and then pass the
log message through unchanged. For @stderrHandler@ I also render the
urgency information, but I also transform the text into all caps -
I really don't want to miss those messages!

@
withSplitLogging :: 'LoggingT' ('WithUrgency' 'Text') 'IO' () -> 'IO' ()
withSplitLogging m =
  'withFDHandler' 'stderr' $ \stderrHandler ->
  'withFDHandler' 'stdout' $ \stdoutHandler ->
  'runLoggingT' m
              ('choose' (\message ->
                        case 'msgUrgency' message of
                          'Error' -> 'Left' message
                          _ -> 'Right' message)
                      ('contramap' ('renderWithUrgency' 'id') stdoutHandler)
                      ('contramap' ('renderWithUrgency' (T.'T.map' 'toUpper')) stdoutHandler))
@

-}

module Control.Monad.Log
       ( -- * @MonadLog@
         MonadLog(..), mapLogMessage,

         -- * @LoggingT@, a general handler
         LoggingT, pattern LoggingT, runLoggingT,
         Handler, pattern Handler, withFDHandler,

         -- * Message wrappers
         WithTimestamp(..), withTimestamps, renderWithUrgency,
         WithUrgency(..), Urgency(..), renderWithTimestamp
       ) where

import Data.Char
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Chan (newChan, writeChan, getChanContents)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.IO (Handle, stderr, stdout)

--------------------------------------------------------------------------------
-- | The class of monads that support logging.
class Monad m => MonadLog message m | m -> message where
  -- | Append a message to the log for this computation.
  logMessage :: message -> m ()

-- | Re-interpret the log messages in one computation. This can be useful to
-- embed a computation with one log type in a larger general computation.
mapLogMessage :: MonadLog message' m => (message -> message') -> LoggingT message m a -> m a
mapLogMessage f m = runLoggingT m (Handler (logMessage . f))

--------------------------------------------------------------------------------
-- | Add \"urgency\" information to a log message. This is often used to convey
-- how significant a log message is.
data WithUrgency a = WithUrgency { msgUrgency :: Urgency, discardUrgency :: a }

data Urgency = Info | Debug | Warning | Error
  deriving (Show)

renderWithUrgency :: (a -> Text) -> (WithUrgency a -> Text)
renderWithUrgency k (WithUrgency u a) = "[" <> pack (show u) <> "] " <> k a

--------------------------------------------------------------------------------
newtype WithTimestamp a = WithTimestamp { unWithTimestamp :: (UTCTime,a) }

renderWithTimestamp :: (a -> Text) -> (WithTimestamp a -> Text)
renderWithTimestamp k (WithTimestamp (t,a)) = pack (show t) <> " " <> k a

-- | Add timestamps to all messages logged. Timestamps will be calculated
-- synchronously when log entries are emitted.
withTimestamps :: (MonadLog (WithTimestamp message) m, MonadIO m) => LoggingT message m a -> m a
withTimestamps m = runLoggingT m
                             (Handler (\msg -> do now <- liftIO getCurrentTime
                                                  logMessage (WithTimestamp (now,msg))))

--------------------------------------------------------------------------------
-- Ap, y u no in base!?
newtype Ap m = Ap { unAp :: m () }

instance Applicative m => Monoid (Ap m) where
  mempty = Ap (pure ())
  mappend (Ap l) (Ap r) = Ap (l *> r)

--------------------------------------------------------------------------------
-- | 'LoggingT' is a very general handler for the 'MonadLog' effect. Whenever a
-- log entry is emitted, the given 'Handler' is invoked, producing some
-- side-effect (such as writing to @stdout@, or appending a database table).
newtype LoggingT message m a = MkLoggingT { unLoggingT :: ReaderT (Handler m message) m a }
  deriving (MonadIO, Monad, Applicative, Functor)

pattern LoggingT f = MkLoggingT (ReaderT f)

instance Monad m => MonadLog message (LoggingT message m) where
  logMessage m = LoggingT (\(Handler f) -> f m)

runLoggingT :: LoggingT message m a -> Handler m message -> m a
runLoggingT (LoggingT m) handler = m handler

--------------------------------------------------------------------------------
-- | Handlers are mechanisms to interpret the meaning of logging as an action
-- in the underlying monad. The instances of 'Contravariant', 'Decidable' and
-- 'Divisible' can all be used to adapt 'Handler's so they can be used in
-- different contexts.
newtype Handler m message = MkHandler { unHandler ::  Op (Ap m) message }
  deriving (Contravariant, Decidable, Divisible)

pattern Handler x <- (fmap unAp . getOp . unHandler -> x)
  where Handler f = MkHandler (Op (Ap . f))

-- | 'newFDHandler' creates a new 'Handler' that will append a given file
-- descriptor (or 'Handle', as it is known in the "base" library). Note that
-- this 'Handler' requires log messages to be of type 'Text'. You can use
-- 'contramap', 'divide' and 'choose' to adapt 'Handler's appropriately.
--
-- These 'Handler's asynchronously log messages to the given file descriptor,
-- rather than blocking.
withFDHandler :: MonadIO io => Handle -> (Handler io Text -> io a) -> io a
withFDHandler fd k = do
  channel <- liftIO newChan
  publisher <- liftIO (async (do messages <- getChanContents channel
                                 traverse (hPutStrLn fd) messages))
  -- TODO Let the publisher die naturally when it has exhausted the channel.
  -- TODO Bracket
  k (Handler (liftIO . writeChan channel)) <* liftIO (cancel publisher)

--------------------------------------------------------------------------------
-- | A 'MonadLog' handler that simply discards all logging requests.
newtype NoLoggingT message m a = NoLoggingT { withoutLogging :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadLog message (NoLoggingT message m) where
  logMessage _ = NoLoggingT (pure ())

--------------------------------------------------------------------------------

-- Test cases

testApp :: MonadLog (WithUrgency Text) m => m ()
testApp = do logMessage (WithUrgency Info "Don't mind me")
             logMessage (WithUrgency Error "But do mind me!")

withSplitLogging :: LoggingT (WithUrgency Text) IO () -> IO ()
withSplitLogging m =
  withFDHandler stderr $ \stderrHandler ->
  withFDHandler stdout $ \stdoutHandler ->
  runLoggingT m
              (choose (\message ->
                        case msgUrgency message of
                          Error -> Left message
                          _ -> Right message)
                      (contramap (renderWithUrgency id) stdoutHandler)
                      (contramap (renderWithUrgency (T.map toUpper)) stdoutHandler))
