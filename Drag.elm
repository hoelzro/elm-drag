module Drag exposing (Model, Msg, initialModel, subscriptions, update)

{-| This module listens for mouse events and creates drag events that
contain the delta x and y of the mouse's movement when the button is
pressed down.

# TEA Data Structures
@docs Model, Msg

# TEA Functions
@docs initialModel, subscriptions, update

    EXAMPLE
-}

import Task
import Time

import Mouse

{-| The internal state of the drag module.  You don't need to poke into this
at all; you just need to make sure it's present in your application's model
and that you update it when Drag events occur.
-}
type alias Model = {
    isDown : Bool,
    currPosition : (Int, Int)
  }

{-| Internal messages for the drag module.  You need to make sure a
constructor for your application's Msg type exists that wraps this,
and that you handle it by calling Drag.update to update the drag
model in your application's update function.
-}
type Msg =
  MouseUp   Mouse.Position |
  MouseDown Mouse.Position |
  MouseMove Mouse.Position

{-| The initial state for the drag module.  Use in your application's
init function to initialize the drag model part of your model.
-}
initialModel : Model
initialModel = {
    isDown = False,
    currPosition = (0, 0)
  }

{-| Returns a subscription for the events that elm-drag needs to function.
The first argument is a constructor that converts elm-drag messages into
your application's messages; usually you'll have a `type Msg = DragMsg Drag.Msg`
constructor, so this argument would just be `DragMsg`.  The second argument
is the drag model.
-}
subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions constructor model =
  let ups   = Mouse.ups <| constructor << MouseUp
      downs = Mouse.downs <| constructor << MouseDown
      moves = Mouse.moves <| constructor << MouseMove
      subs = if model.isDown then [ ups, downs, moves ] else [ downs ]
  in Sub.batch subs

dragCmd : ((Int, Int) -> msg) -> (Int, Int) -> (Int, Int) -> Cmd msg
dragCmd constructor (px, py) (cx, cy) =
  let dx = cx - px
      dy = cy - py
      task = always <| constructor (dx, dy)
  in Task.perform task task Time.now

{-| Updates the drag model.  The first argument is a function that
converts an `(Int, Int)` to your application's message type; if you
have a `type Msg = Drag (Int, Int)`, this would just be `Drag`.
The second and third arguments are the drag message and model that
you're currently processing.
-}
update : ((Int, Int) -> msg) -> Msg -> Model -> (Model, Cmd msg)
update constructor msg model =
  case msg of
    MouseUp _        -> ({ model | isDown = False }, Cmd.none)
    MouseDown {x, y} -> ({ isDown = True, currPosition = (x, y) }, Cmd.none)
    MouseMove {x, y} ->
      let newModel = { model | currPosition = (x, y) }
          cmd = if model.isDown
            then dragCmd constructor model.currPosition (x, y)
            else Cmd.none
      in (newModel, cmd)
