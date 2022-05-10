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

type alias Position = (Float, Float)
type alias Velocity = (Float, Float)
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
    hero = Model 400 600 { position = (5, 50), velocity = (0, 0) }
  in
    (hero, Cmd.none)


-- UPDATE


type Msg
  = Tick Time.Posix
  | BodyKeyPress Int

move: Hero -> Float -> Float -> Hero
move hero x y =
  { hero | position = (x, y)}

type Direction = Left | Right | Up | Down

decelerate : Float -> Float -> Float
decelerate speed resistance =
  if abs speed <= 0.01 then 0 else speed * (1 - resistance / 60)

withFriction : Hero -> Hero
withFriction hero =
  let (vx, vy) = hero.velocity in
  let friction = 0.1 in
  { hero | velocity = (decelerate vx friction, decelerate vy friction) }

push : Hero -> Direction -> Model -> Hero
push hero direction {height, width} =
  let (x, y) = hero.velocity in
  let (px, py) = hero.position in
    case direction of
      Left -> 
        let speed = if px > 0 then x - 0.2 else 0 in
          { hero | velocity = (speed, y)}
      Right ->
        let speed = if px < toFloat width then x + 0.2 else 0 in
          { hero | velocity = (speed, y)}
      Up ->
        let speed = if py > 0 then y - 0.2 else 0 in
          { hero | velocity = (x, speed)}
      Down ->
        let speed = if py < toFloat height then y + 0.2 else 0 in
        { hero | velocity = (x, speed)}

clamped : Float -> Float -> Position -> Position 
clamped maxX maxY (x, y) =
  (clamp 0 maxX x, clamp 0 maxY y)

nextState model =
  let {position, velocity} = model.hero in
  let ((x, y), (vx, vy)) = (position, velocity) in
  let (nextX, nextY) = clamped (toFloat model.width) (toFloat model.height) (x + vx, y + vy) in
  { model | hero = move model.hero nextX nextY |> withFriction }

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick _ ->
      let next = nextState model in
        (next, sendMessage (encodeGame next))
    BodyKeyPress keyCode ->
      case keyCode of
        37 -> let {hero} = model in ({ model | hero = model |> push hero Left }, Cmd.none)
        38 -> let {hero} = model in ({ model | hero = model |> push hero Up }, Cmd.none)
        39 -> let {hero} = model in ({ model | hero = model |> push hero Right }, Cmd.none)
        40 -> let {hero} = model in ({ model | hero = model |> push hero Down }, Cmd.none)
        _ -> (model, Cmd.none)

-- VIEW

encodeHero : Hero -> E.Value
encodeHero {position} = 
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.float x)
      , ("y", E.float y)
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