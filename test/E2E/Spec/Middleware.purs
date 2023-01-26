module Test.E2E.Spec.Middleware where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Const (Const)
import Effect.Aff (Aff, Error)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Test.E2E.Environment (logShouldBe)
import Test.E2E.Environment as Environment
import Test.E2E.Environment.Middleware (_component, _incrementButton, _state, app)
import Test.E2E.Environment.Store (Action(..))
import Test.E2E.Logger (VirtualConsole, resetLog)
import Test.Spec (Spec, before, describe, it)
import Test.Utils.HTML (unsafeQuerySelector, unsafeToHTMLElement, (.>))
import Test.Utils.HTML as HTML
import Web.DOM (Element)
import Web.HTML (Window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

type TestEnv =
  { window :: Window
  , console :: VirtualConsole
  , io :: H.HalogenIO (Const Unit) Void Aff
  , elements ::
      { state :: Element
      , incrementBtn :: Element
      }
  }

setup :: Aff TestEnv
setup = do
  { window, console, io } <- Environment.setup app
  pn <- liftEffect $ HTMLDocument.toParentNode <$> document window
  elements <- { state: _, incrementBtn: _ }
    <$> unsafeQuerySelector (_component .> _state) pn
    <*> unsafeQuerySelector (_component .> _incrementButton) pn
  pure
    { window
    , console
    , io
    , elements
    }

textContentShouldBe :: forall m a. MonadEffect m => MonadThrow Error m => Show a => Element -> a -> m Unit
textContentShouldBe el = show >>> (el `HTML.textContentShouldBe` _)

spec :: Spec Unit
spec = before setup do
  describe "Single component with Middleware" do
    it "should log dispatched action and state information via middleware" \{ console, io, elements: el } -> do
      let click = unsafeToHTMLElement >=> HTMLElement.click >>> liftEffect

      resetLog console

      click el.incrementBtn
      console `logShouldBe`
        [ stateLoggerLogMessageBef 0 false
        , actionLoggerLogMessage Increment
        , stateLoggerLogMessageAft 1 false
        ]

      click el.incrementBtn
      click el.incrementBtn

      console `logShouldBe`
        [ stateLoggerLogMessageBef 0 false
        , actionLoggerLogMessage Increment
        , stateLoggerLogMessageAft 1 false
        ---
        , stateLoggerLogMessageBef 1 false
        , actionLoggerLogMessage Increment
        , stateLoggerLogMessageAft 2 false
        ---
        , stateLoggerLogMessageBef 2 false
        , actionLoggerLogMessage Increment
        , stateLoggerLogMessageAft 3 false
        --- after clicking increment button three times and count value reaches 3,
        --- toggleDispatcher middleware dispatches Toggle action
        , stateLoggerLogMessageBef 3 false
        , actionLoggerLogMessage Toggle
        , stateLoggerLogMessageAft 3 true
        ]

      io.dispose :: Aff Unit

  where
  stateLoggerLogMessageBef count switch = "Before state: " <> show { count, switch }
  stateLoggerLogMessageAft count switch = "After state: " <> show { count, switch }
  actionLoggerLogMessage act = "Dispatched: " <> show act
