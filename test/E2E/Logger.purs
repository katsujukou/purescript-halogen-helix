module Test.E2E.Logger
  ( Logger
  , LoggerT(..)
  , VirtualConsole
  , class MonadLogger
  , readLog
  , resetLog
  , runLoggerT
  , writeLog
  , writeLogLn
  , clearLog
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Writer (class MonadTell, class MonadTrans, class MonadWriter, WriterT, lift, runWriterT)
import Control.Monad.Writer as W
import Data.Array as Array
import Data.List (List)
import Data.List as L
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Hooks as Hooks

class MonadEffect m <= MonadLogger m where
  writeLog :: List String -> m Unit
  clearLog :: m Unit

instance MonadLogger m => MonadLogger (Hooks.HookM m) where
  writeLog = writeLog >>> lift
  clearLog = lift clearLog

writeLogLn :: forall m. MonadLogger m => String -> m Unit
writeLogLn = L.singleton >>> writeLog

newtype LoggerT m a = LoggerT (WriterT (List String) m a)

derive newtype instance Functor m => Functor (LoggerT m)
derive newtype instance Apply m => Apply (LoggerT m)
derive newtype instance Applicative m => Applicative (LoggerT m)
derive newtype instance Bind m => Bind (LoggerT m)
derive newtype instance Monad m => Monad (LoggerT m)
derive newtype instance MonadEffect m => MonadEffect (LoggerT m)
derive newtype instance MonadAff m => MonadAff (LoggerT m)
derive newtype instance Monad m => MonadTell (List String) (LoggerT m)
derive newtype instance Monad m => MonadWriter (List String) (LoggerT m)
derive newtype instance MonadThrow e m => MonadThrow e (LoggerT m)
derive newtype instance MonadError e m => MonadError e (LoggerT m)
derive newtype instance MonadTrans LoggerT

instance MonadEffect m => MonadLogger (LoggerT m) where
  writeLog = W.tell >>> LoggerT
  clearLog = LoggerT $ W.pass (pure $ Tuple unit (const L.Nil))

type VirtualConsole = Ref (List String)

runLoggerT :: forall m. MonadEffect m => VirtualConsole -> LoggerT m ~> m
runLoggerT vc (LoggerT m) = runWriterT m >>= \(a /\ logs) -> (liftEffect $ Ref.modify (_ <> logs) vc) $> a

type Logger = LoggerT Aff

readLog :: forall m. MonadEffect m => VirtualConsole -> m (Array String)
readLog = map Array.fromFoldable <<< liftEffect <<< Ref.read

resetLog :: forall m. MonadEffect m => VirtualConsole -> m Unit
resetLog = liftEffect <<< Ref.write L.Nil