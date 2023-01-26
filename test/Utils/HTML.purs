module Test.Utils.HTML where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Maybe (maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Test.Spec.Assertions (shouldEqual)
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)
import Web.DOM.ParentNode (ParentNode, QuerySelector(..))
import Web.DOM.ParentNode as DOM
import Web.HTML.HTMLElement as HTMLElement

childCombinator :: QuerySelector -> QuerySelector -> QuerySelector
childCombinator (QuerySelector p) (QuerySelector c) = QuerySelector $ p <> " > " <> c

infixr 4 childCombinator as .>

textContentShouldBe :: forall m. MonadEffect m => MonadThrow Error m => Element -> String -> m Unit
textContentShouldBe el content = do
  c <- liftEffect $ textContent $ Element.toNode el
  c `shouldEqual` content

unsafeQuerySelector :: forall m. MonadEffect m => MonadThrow Error m => QuerySelector -> ParentNode -> m Element
unsafeQuerySelector selector node = liftEffect (DOM.querySelector selector node)
  >>= maybe (throwError $ error "Element not found") pure

unsafeToHTMLElement :: forall m. MonadThrow Error m => Element -> m HTMLElement.HTMLElement
unsafeToHTMLElement =
  HTMLElement.fromElement
    >>> maybe (throwError $ error "Not a HTMLElement") pure
