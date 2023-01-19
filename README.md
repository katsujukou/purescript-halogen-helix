# PureScript Halogen Helix

Lightweight global state management for PureScript Halogen.

## What is this?

**PureScript Halogen Helix** is a global state management library for Halogen app.

At the time of writing, there already exists a great library sharing the same purpose: [halogen-store](https://github.com/thomashoneyman/purescript-halogen-store), but this library differs in several points:

- This library only supports hook style components
- Instead of having a single large store, you can define small stores as many as you want.
- No typeclass, no transformers

## Quick Start

You start by defining a store. As with [halogen-store](https://github.com/thomashoneyman/purescript-halogen-store) or other similar libraries, a Helix store consist of three parts: _State, Action, and Reducer_.

```purs
type State = Int

data Action = Increment | Decrement

reducer :: State -> Action -> State
reducer st = case _ of
  Increment -> st + 1
  Decrement -> st - 1
```

And then you use the `makeStore` function from this library. Note that `makeStore` itself is not a hook function, but a **higher-order hook**, because it accepts some inputs and produce a hook. The first argumnt is the **unique** identifier of your store. The second argument is the initial state value.

**Please be sure not to create multiple stores of the same ID, because doing so will bring things to mess up.**

```purs
import Halogen.Helix (makeStore, UseHelix)

useCounter :: forall ctx m. MonadEffect m => Eq ctx => UseHelixHook State Action ctx m
useCounter = makeStore "counter" 0 reducer
```

It's now time to connect your Halogen components to the store!
To do that, it is as simple as calling your hook returned by the `makeStore`.
The hook accepts single argument `selector`, which has a type `State -> part`, selects the part of the store visible to the component.
In this case, the `State` type is `Int`, and we want our component to have access to the entire store, so we provide `identity`.

```purs
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

counter :: forall q i o m. MonadEffect m => H.Component q i o m
conuter = Hooks.component \_ _ -> Hooks.do
  state /\ ctx <- useCounter identity
```

`useCounter` hook returns two things:

1. `state`, the current state value
2. `ctx`, the _store context object_.

As with other hooks, the `state` is pure value and intended only to be used in the render function.

The context object contains two functions:

- `getState :: HookM m state` ... returns the current value of the (selected part of the) state.
- `dispatch :: action -> HookM m Unit` ... accepts the `Action` value and dispatches it to the store.

Here is our completed example:

```purs
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

For a bit more realistic example, please refer to the `example` director, which contains a very simple Todo app.

## Run example app

In the project root,

```sh
npm i
npm run example
```

Open your favorite browser and go to http://localhost:5173.
