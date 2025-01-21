# PureScript Halogen Helix

Lightweight global state management for PureScript Halogen.

[![purs - v0.15.15](https://img.shields.io/badge/purs-v0.15.15-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.15) [![CI](https://github.com/katsujukou/purescript-halogen-helix/actions/workflows/ci.yml/badge.svg)](https://github.com/katsujukou/purescript-halogen-helix/actions/workflows/ci.yml)

## What is this?

**PureScript Halogen Helix** is a global state management library for Halogen apps.

At the time of writing, there already exists a great library sharing the same purpose: [halogen-store](https://github.com/thomashoneyman/purescript-halogen-store), but this library differs in several points:

- This library only supports hook style components.
- Instead of having a single large store, you can define small stores as many as you want.
- You can extend store functionality via middlewares.
- No typeclass, no transformers

## Quick Start

We start by defining a store. As with [halogen-store](https://github.com/thomashoneyman/purescript-halogen-store) or other similar libraries, a Helix store consist of three parts: _State, Action, and Reducer_.

```purs
type State = Int

data Action = Increment | Decrement

reducer :: State -> Action -> State
reducer st = case _ of
  Increment -> st + 1
  Decrement -> st - 1
```

And then we use the `makeStore'` function from this library. Note that `makeStore'` itself is not a hook function, but a **higher-order hook**, because it accepts some inputs and produce a hook function. The first argument is the **unique** identifier of our store. The third argument is the initial state value.

**Important:** Avoid creating multiple stores with the same ID to prevent conflicts.

```purs
import Halogen.Helix (makeStore', UseHelix)

useCounter :: forall ctx m. MonadEffect m => Eq ctx => UseHelixHook State Action ctx m
useCounter = makeStore' "counter" reducer 0
```

It's now time to connect our Halogen components to the Helix store!
To do that, it is as simple as calling our hook returned by the `makeStore'`.
The hook function accepts single argument `selector`, which has a type `State -> part`, selects the part of the store visible to the component.
In this case, the `State` type is `Int`, and we want our component to have access to the entire store, so we provide `identity`.

```purs
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

counter :: forall q i o m. MonadEffect m => H.Component q i o m
counter = Hooks.component \_ _ -> Hooks.do
  state /\ ctx <- useCounter identity
```

_Note `/\` is an operator for creating `Tuple`, here used to unpack the returned `Tuple`_

`useCounter` hook returns two things:

1. `state`, the current state value
2. `ctx`, the _store context object_.

As with other hooks, the `state` is pure value and intended only to be used in the render function.

The context object contains two functions:

- `getState :: HookM m state` ... returns the current value (selected part) of the state.
- `dispatch :: action -> HookM m Unit` ... accepts the `Action` value and dispatches it to the Helix store manager. Behind the scene, the Helix store manager calculates the next state value using the dispatched action and reducer, updates the state with that value.

Here is our completed example:

```purs
counter :: forall q i o m. MonadEffect m => H.Component q i o m
counter = Hooks.component \_ _ -> Hooks.do
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

For a bit more realistic example, please refer to the `example` directory, which contains a very simple Todo app.

## Middlewares

The Helix store is kept intentionally simple; we can manipulate the store in a very restricted manner - the only thing we can do to interact with store is to dispatch an action, and calculating of the next state is a pure computation. Sometimes, however, it might be necessary to do more than such a thing, and state manipulation might be accompanied by some effectful tasks, especially in a real-world app.

For such a needs, Helix offers the way to extend store's functionality via almost arbitrary effectful computation: **Middlewares**.
A Helix middleware is a bunch of effectful computations which can be inserted in various points in _dispatching - reducer - state update_ flow.
Via middlewares, we can do almost arbitrary effectful task, as long as it is captured by the `HookM m` monad. For instance, we can do:

- logging a value to the console
- generating random unieuq IDs (such as UUID)
- making an AJAX call
- controlling store manupulation flow by canceling state update or dispatching other actions conditionally

A Helix middleware is just a function of three arguments:

```purs
type HelixMiddleware state action m
   = HelixContext state action m
  -> action
  -> (action -> m Unit)
  -> m Unit

myMiddleware :: HelixMiddleware State Action m
myMiddleware = \ctx action next -> ...
```

- `ctx` is a Helix context object, enabling us to access the store via `getState` and dispatching further actions via `dispatch`.
- `action` is a dispatched action.
- `next` is a next middleware. Middlewares can be added as many as you need, forming the middleware stack. By calling `next` with `action`, we can delegate the process to the next layer middleware.

For instance, you can do logging dispatched action to the console via a middleware like this:

```purs
actionLogger :: HelixMiddleware state action m
actionLogger ctx action next = do
  Console.log $ "Action dispatched: " <> show action
  next action
```

A middleware like this gives ability to logging the state value before and after updates:

```purs
stateLogger :: HelixMiddleware state action m
stateLogger ctx action next = do
  ctx.getState >>= show >>> ("Before state: " <> _) >>> Console.log
  next action
  ctx.getState >>= show >>> ("After state: " <> _) >>> Console.log
```

and you can combine two middlewares by `|>` operator:

```purs
import Halogen.Helix (HelixMiddleware, (|>))

middlewareStack :: HelixMiddleware state action m
middlewareStack = stateLogger |> actionLogger
```

You can apply your middleware to your store by using non-prime `makeStore`:

```purs
useCounter :: forall m s. MonadEffect m => Eq s => UseHelixHook State Action s m
useCounter = makeStore "counter" reducer 0 middlewareStack
```

For more realistic usecase, please see the example app.

## Run example app

In the project root,

```sh
npm i
npm run example
```

Open your favorite browser and go to `http://localhost:5173`.
