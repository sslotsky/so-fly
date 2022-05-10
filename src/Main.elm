port module Main exposing (..)
import Json.Encode as E
import Time
import Browser
import Html exposing (Html, node)
import Html.Attributes exposing (height)
import Html.Attributes exposing (width)



-- MAIN


main : Program () Model Msg
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions : a -> Sub Msg
subscriptions _ =
  Sub.batch 
    [ Time.every 1 Tick
    , bodyKeyPress BodyKeyPress
    ]

port sendMessage : E.Value -> Cmd msg
port bodyKeyPress : (Int -> msg) -> Sub msg


-- MODEL

type alias Position = (Int, Int)
type alias Hero =
  { position: Position }
type alias Model =
  { height: Int
  , width: Int
  , hero: Hero
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    hero = Model 200 600 { position = (5, 50) }
  in
    (hero, Cmd.none)


-- UPDATE


type Msg
  = Tick Time.Posix
  | BodyKeyPress Int


-- update : Msg -> Model -> Model
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  let
    (x, y) = model.hero.position in
  case msg of
    Tick _ ->
      (model, sendMessage (encodeGame model))
    BodyKeyPress keyCode ->
      case keyCode of
        37 -> (Model model.height model.width { position = (x - 1, y)}, Cmd.none)
        _ -> (model, Cmd.none)

-- VIEW

encodeHero : Hero -> E.Value
encodeHero {position} = 
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.int x)
      , ("y", E.int y)
      ]
encodeGame : { a | height : Int, width : Int, hero : Hero } -> E.Value
encodeGame model =
  E.object
    [ ("height", E.int model.height)
    , ("width", E.int model.width)
    , ("hero", encodeHero model.hero)
    ]

view : Model -> Html Msg
view model =
  node "game-canvas" [height model.height, width model.width] []