module Test.E2E.Environment where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Halogen (HalogenIO)
import Halogen as H
import Halogen.VDom.Driver (runUI)
import Test.E2E.Logger (Logger, VirtualConsole, runLoggerT)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils.HappyDOM as HappyDOM
import Web.HTML (Window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

type Env q o =
  { io :: HalogenIO q o Aff
  , window :: Window
  , console :: VirtualConsole
  }

setup :: forall q o. H.Component q {} o Logger -> Aff (Env q o)
setup app = do
  window <- liftEffect HappyDOM.window
  mbBody <- liftEffect (HTMLDocument.body =<< document window)
  case mbBody of
    Just body -> do
      console <- liftEffect $ Ref.new Nil
      io <- runUI (H.hoist (runLoggerT console) app) {} body
      pure { io, window, console }
    Nothing -> liftEffect $ throw "Failed to setup test environment."

logShouldBe :: forall m. MonadEffect m => MonadThrow Error m => VirtualConsole -> Array String -> m Unit
logShouldBe vc expect = do
  log <- liftEffect $ Array.fromFoldable <$> Ref.read vc
  log `shouldEqual` expect