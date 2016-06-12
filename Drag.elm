module Drag exposing (Model, Msg(..), initialModel, subscriptions, update)

import Task
import Time

import Mouse

type alias Model = {
    isDown : Bool,
    wasDown : Bool,
    currPosition : (Int, Int),
    prevPosition : (Int, Int)
  }

type Msg =
  MouseUp   Mouse.Position |
  MouseDown Mouse.Position |
  MouseMove Mouse.Position

initialModel : Model
initialModel = {
    isDown = False,
    wasDown = False,
    currPosition = (0, 0),
    prevPosition = (0, 0)
  }

subscriptions : Model -> (Msg -> msg) -> Sub msg
subscriptions model constructor =
  Sub.batch [
      Mouse.ups   <| constructor << MouseUp,
      Mouse.downs <| constructor << MouseDown,
      Mouse.moves <| constructor << MouseMove
    ]

dragCmd : ((Int, Int) -> msg) -> (Int, Int) -> (Int, Int) -> Cmd msg
dragCmd constructor (px, py) (cx, cy) =
  let dx = px - cx
      dy = py - cy
      task = always <| constructor (dx, dy)
  in Task.perform task task Time.now

update : Msg -> Model -> ((Int, Int) -> msg) -> (Model, Cmd msg)
update msg model constructor =
  case msg of
    MouseUp _        -> ({ model | isDown = False, wasDown = model.isDown }, Cmd.none)
    MouseDown {x, y} -> ({ isDown = True, wasDown = model.isDown, currPosition = (x, y), prevPosition = (x, y) }, Cmd.none)
    MouseMove {x, y} -> ({ model | currPosition = (x, y), prevPosition = model.currPosition }, if model.isDown then dragCmd constructor model.currPosition (x, y) else Cmd.none)
