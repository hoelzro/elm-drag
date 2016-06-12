import Html.App as App
import Html exposing (Html, text)

import Mouse
import Task
import Time

type alias Model = {
    isDown : Bool,
    wasDown : Bool,
    currPosition : (Int, Int),
    prevPosition : (Int, Int),
    dragDistance : Int
  }

type Msg =
  MouseUp   Mouse.Position |
  MouseDown Mouse.Position |
  MouseMove Mouse.Position |
  Drag (Int, Int)

dragCmd : ((Int, Int) -> msg) -> (Int, Int) -> (Int, Int) -> Cmd msg
dragCmd constructor (px, py) (cx, cy) =
  let dx = px - cx
      dy = py - cy
      task = always <| constructor (dx, dy)
  in Task.perform task task Time.now

init : (Model, Cmd Msg)
init =
  let initialModel = {
    isDown = False,
    wasDown = False,
    currPosition = (0, 0),
    prevPosition = (0, 0),
    dragDistance = 0
  } in (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseUp _        -> ({ model | isDown = False, wasDown = model.isDown }, Cmd.none)
    MouseDown {x, y} -> ({ model | isDown = True, wasDown = model.isDown, currPosition = (x, y), prevPosition = (x, y) }, Cmd.none)
    MouseMove {x, y} -> ({ model | currPosition = (x, y), prevPosition = model.currPosition }, if model.isDown then dragCmd Drag model.currPosition (x, y) else Cmd.none)
    Drag (dx, dy)    -> ({ model | dragDistance = model.dragDistance + (abs dx) + (abs dy) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [
    Mouse.ups   MouseUp,
    Mouse.downs MouseDown,
    Mouse.moves MouseMove
  ]

view : Model -> Html Msg
view model = text <| toString model

main : Program Never
main = App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
