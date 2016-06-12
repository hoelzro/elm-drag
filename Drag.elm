module Drag exposing (Model, Msg(..), initialModel, subscriptions, update)

import Task
import Time

import Mouse

type alias Model = {
    isDown : Bool,
    currPosition : (Int, Int)
  }

type Msg =
  MouseUp   Mouse.Position |
  MouseDown Mouse.Position |
  MouseMove Mouse.Position

initialModel : Model
initialModel = {
    isDown = False,
    currPosition = (0, 0)
  }

subscriptions : Model -> (Msg -> msg) -> Sub msg
subscriptions model constructor =
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

update : Msg -> Model -> ((Int, Int) -> msg) -> (Model, Cmd msg)
update msg model constructor =
  case msg of
    MouseUp _        -> ({ model | isDown = False }, Cmd.none)
    MouseDown {x, y} -> ({ isDown = True, currPosition = (x, y) }, Cmd.none)
    MouseMove {x, y} ->
      let newModel = { model | currPosition = (x, y) }
          cmd = if model.isDown
            then dragCmd constructor model.currPosition (x, y)
            else Cmd.none
      in (newModel, cmd)
