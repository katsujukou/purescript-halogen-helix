module Test.Store where

import Prelude

type State = { count :: Int, switch :: Boolean }

data Action = Increment | Decrement | Toggle

reducer :: State -> Action -> State
reducer st = case _ of
  Increment -> st { count = st.count + 1 }
  Decrement -> st { count = st.count - 1 }
  Toggle -> st { switch = not st.switch }