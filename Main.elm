import Html.App as App
import Html exposing (Html, text)

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

init : (Model, Cmd Msg)
init =
  let initialModel = {
    isDown = False,
    wasDown = False,
    currPosition = (0, 0),
    prevPosition = (0, 0)
  } in (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

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
