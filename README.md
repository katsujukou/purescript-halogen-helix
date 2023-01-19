# PureScript Halogen Helix

Lightweight global state management for PureScript Halogen.

## Quick Start

```purs
type State = Int

data Action = Increment | Decrement

reducer :: State -> Action -> State
reducer st = case _ of
  Increment -> st + 1
  Decrement -> st - 1
```

```purs
import Halogen.Helix (makeStore, UseHelix)

useCounter :: forall ctx m. MonadEffect m => Eq ctx => UseHelixHook State Action ctx m
useCounter = makeStore "counter" 0 reducer
```

```purs
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

counter :: forall q i o m. MonadEffect m => H.Component q i o m
conuter = Hooks.component \_ _ -> Hooks.do
  state /\ ctx <- useCounter identity

  Hooks.pure do
    HH.div_
      [ HH.div_
        [ HH.text $ "Counter: " <> show state ]
      , HH.div_
        [ HH.button
          [ HP.type_ HP.ButtonButton
          , HE.onClick \_ -> ctx.dispatch Decrement
          ]
          [ HH.text  "-1"
          ]
        , HH.button
          [ HP.type_ HP.ButtonButton
          , HE.onClick \_ -> ctx.dispatch Increment
          ]
          [ HH.text  "+1"
          ]
        ]
      ]
```

## Run example app

In the project root,

```sh
npm i
npm run example
```

Open your favorite browser and go to http://localhost:5173.

## Differences to [halogen-store](https://github.com/thomashoneyman/purescript-halogen-store)
