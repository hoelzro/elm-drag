module Drag exposing (Model, Msg(..), initialModel, subscriptions, update)

{-| Module documentation.

@docs Model, Msg, initialModel, subscriptions, update
-}

import Task
import Time

import Mouse

{-| Model documentation.
-}
type alias Model = {
    isDown : Bool,
    currPosition : (Int, Int)
  }

{-| Msg documentation.
-}
type Msg =
  MouseUp   Mouse.Position |
  MouseDown Mouse.Position |
  MouseMove Mouse.Position

{-| initialModel documentation.
-}
initialModel : Model
initialModel = {
    isDown = False,
    currPosition = (0, 0)
  }

{-| subscriptions documentation.
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
  let dx = px - cx
      dy = py - cy
      task = always <| constructor (dx, dy)
  in Task.perform task task Time.now

{-| update documentation.
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
