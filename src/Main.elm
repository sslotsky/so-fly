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
type alias Velocity = (Int, Int)
type alias Hero =
  { position: Position, velocity: Velocity }
type alias Model =
  { height: Int
  , width: Int
  , hero: Hero
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    hero = Model 200 600 { position = (5, 50), velocity = (0, 0) }
  in
    (hero, Cmd.none)


-- UPDATE


type Msg
  = Tick Time.Posix
  | BodyKeyPress Int

move: Hero -> Int -> Int -> Hero
move hero x y =
  { hero | position = (x, y)}

type Direction = Left | Right | Up | Down

push : Hero -> Direction -> Hero
push hero direction =
  let (x, y) = hero.velocity in
    case direction of
      Left -> { hero | velocity = (x - 1, y)}
      Right -> { hero | velocity = (x + 1, y)}
      Up -> { hero | velocity = (x, y - 1)}
      Down -> { hero | velocity = (x, y + 1)}

nextState model =
  let {position, velocity} = model.hero in
  let ((x, y), (vx, vy)) = (position, velocity) in
  { model | hero = move model.hero (x + vx) (y + vy)}

-- update : Msg -> Model -> Model
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick _ ->
      let next = nextState model in
        (next, sendMessage (encodeGame next))
    BodyKeyPress keyCode ->
      case keyCode of
        37 -> let {hero} = model in ({ model | hero = push hero Left }, Cmd.none)
        38 -> let {hero} = model in ({ model | hero = push hero Up }, Cmd.none)
        39 -> let {hero} = model in ({ model | hero = push hero Right }, Cmd.none)
        40 -> let {hero} = model in ({ model | hero = push hero Down }, Cmd.none)
        -- 37 -> (Model model.height model.width { position = (x - 1, y)}, Cmd.none)
        -- 38 -> (Model model.height model.width { position = (x, y - 1)}, Cmd.none)
        -- 39 -> (Model model.height model.width { position = (x + 1, y)}, Cmd.none)
        -- 40 -> (Model model.height model.width { position = (x, y + 1)}, Cmd.none)
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