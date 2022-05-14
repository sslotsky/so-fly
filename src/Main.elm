port module Main exposing (..)
import Json.Encode as E
import Time
import Browser
import Random
import Html exposing (Html, node)
import Html.Attributes exposing (height)
import Html.Attributes exposing (width)
import Array exposing (get, fromList, toList, set)



-- MAIN


main : Program () Model Msg
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions : a -> Sub Msg
subscriptions _ =
  Sub.batch 
    [ Time.every 1 Tick
    , bodyKeyPress BodyKeyPress
    , bodyKeyUp BodyKeyUp
    ]

port sendMessage : E.Value -> Cmd msg
port bodyKeyPress : (Int -> msg) -> Sub msg
port bodyKeyUp : (Int -> msg) -> Sub msg


-- MODEL

type alias Position = (Float, Float)
type alias Velocity = (Float, Float)

type alias MoveController =
  { right: Bool
  , left: Bool
  , up: Bool
  , down: Bool
  }

type alias Fly =
  { position: Position, velocity: Velocity }
type alias Hero =
  { position: Position, velocity: Velocity, size: Float }

type alias Model =
  { height: Int
  , width: Int
  , score: Int
  , hero: Hero
  , flies: List(Fly)
  , controller: MoveController
  }

init : () -> (Model, Cmd Msg)
init _ =
  let ((height, width), position, velocity) = ((400, 600), (5, 50), (0, 0)) in
  let controller = { right = False, up = False, down = False, left = False } in

  let hero = { position = position, velocity = velocity, size = 1 } in
  let model = Model height width 0 hero [] controller
  in
    (model, Cmd.none)


-- UPDATE


type alias FlySpawn =
  { position: Position
  , odds: Float
  }

type alias TurnMessage =
  { index: Int
  , angle: Int
  , odds: Float
  }
type Msg
  = Tick Time.Posix
  | NewFly FlySpawn
  | BodyKeyPress Int
  | BodyKeyUp Int
  | Turn TurnMessage

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
  let acc = 0.02 in
  let (px, py) = hero.position in
    case direction of
      Left -> 
        let speed = if px > 0 then x - acc else 0 in
          { hero | velocity = (speed, y)}
      Right ->
        let speed = if px < toFloat width then x + acc else 0 in
          { hero | velocity = (speed, y)}
      Up ->
        let speed = if py > 0 then y - acc else 0 in
          { hero | velocity = (x, speed)}
      Down ->
        let speed = if py < toFloat height then y + acc else 0 in
        { hero | velocity = (x, speed)}

adjustVelocity : Model -> Hero
adjustVelocity model =
  let
    {controller} = model
    adjust : Direction -> Hero -> Hero
    adjust direction hero =
      case direction of
        Left -> if controller.left then model |> push hero Left else hero
        Right -> if controller.right then model |> push hero Right else hero
        Up -> if controller.up then model |> push hero Up else hero
        Down -> if controller.down then model |> push hero Down else hero
  in
    model.hero |> adjust Left |> adjust Right |> adjust Up |> adjust Down

clamped : Float -> Float -> Position -> Position 
clamped maxX maxY (x, y) =
  (clamp 0 maxX x, clamp 0 maxY y)

nextState : Model -> Model
nextState model =
  let
    hero = adjustVelocity model
    {position, velocity} = hero
    ((x, y), (vx, vy)) = (position, velocity)
    (nextX, nextY) = clamped (toFloat model.width) (toFloat model.height) (x + vx, y + vy)
    newSize = max (hero.size * 0.9999) 1
  in
    { model | flies = moveFlies model.flies, hero = move { hero | size = newSize } nextX nextY |> withFriction }

moveToggle : Bool -> Direction -> Model -> Model
moveToggle on direction model =
  let {controller} = model in
    case direction of
      Left -> { model | controller = { controller | left = on }}
      Right -> { model | controller = { controller | right = on }}
      Up -> { model | controller = { controller | up = on }}
      Down -> { model | controller = { controller | down = on }}

startMoving = moveToggle(True)
stopMoving = moveToggle(False)

randomPoint : Model -> Random.Generator Position
randomPoint model =
  Random.pair (Random.float 0 (toFloat model.width)) (Random.float 0 (toFloat model.height))

randomFly model =
  Random.map2 FlySpawn (randomPoint model) (Random.float 0 1)

turnFly flies = 
  Random.map3 TurnMessage (Random.int 0 ((List.length flies) - 1)) (Random.int 0 360) (Random.float 0 1)

moveFlies : List(Fly) -> List(Fly)
moveFlies flies =
  flies |> List.map
    (
      \fly ->
        let
          (x, y) = fly.position
          (vx, vy) = fly.velocity
        in
          { fly | position = (x + vx, y + vy)}
    )

captureNearbyFlies : Model -> Model
captureNearbyFlies model =
  let
    isWithinRange : Fly -> Bool
    isWithinRange {position} =
      let
        (x, y) = position
        (heroX, heroY) = model.hero.position
        range = model.hero.size * 20
        gapX = abs (x - heroX)
        gapY = abs (y - heroY)
        distance = sqrt (gapX * gapX) + (gapY * gapY)
      in
        distance <= range
    
    (captured, uncaptured) = model.flies |> List.partition isWithinRange
    {hero} = model
    biggerHero = { hero | size = hero.size + (0.2 * toFloat (List.length captured))}
    
  in
    {
      model |
        score = model.score + (List.length captured)
      , flies = uncaptured, hero = biggerHero
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      let next = nextState model in
        ( next
        , Cmd.batch
          [ sendMessage (encodeGame next)
          , Random.generate NewFly (randomFly model)
          , Random.generate Turn (turnFly model.flies) 
          ]
        )
    NewFly {position, odds} ->
      if odds > 0.005 then
        (model, Cmd.none)
      else
        let
          fly = { position = position, velocity = (0, 0) }
          flies = fly :: model.flies
        in
          ({ model | flies = flies }, Cmd.none)
    BodyKeyPress keyCode ->
      case keyCode of
        37 -> (model |> startMoving Left, Cmd.none)
        38 -> (model |> startMoving Up, Cmd.none)
        39 -> (model |> startMoving Right, Cmd.none)
        40 -> (model |> startMoving Down, Cmd.none)
        32 -> (model |> captureNearbyFlies, Cmd.none)
        _ -> (model, Cmd.none)
    BodyKeyUp keyCode ->
      case keyCode of
        37 -> (model |> stopMoving Left, Cmd.none)
        38 -> (model |> stopMoving Up, Cmd.none)
        39 -> (model |> stopMoving Right, Cmd.none)
        40 -> (model |> stopMoving Down, Cmd.none)
        _ -> (model, Cmd.none)
    Turn {index, angle, odds} ->
      if odds > 0.2 then
        (model, Cmd.none)
      else
        let
          fly = get index (fromList model.flies)
        in
          case fly of
            Nothing -> (model, Cmd.none)
            Just f ->
              let
                slope = tan (degrees (toFloat angle))
                b = 1 / (5 * sqrt(1 + (slope * slope)))
                a = 1 / (5 * sqrt(1 + (1 / (slope * slope))))
                (vx, vy) = case (angle < 180, angle >= 90 && angle < 270) of
                  (True, True) -> (b, -a)
                  (True, False) -> (b, a)
                  (False, True) -> (-b, -a)
                  (False, False) -> (-b, a)
                adjustedFly = { f | velocity = (vx, vy) }
                flies = toList (set index adjustedFly (fromList model.flies))
              in
                ({ model | flies = flies }, Cmd.none)
        

-- VIEW

encodeHero : Hero -> E.Value
encodeHero {position, size} = 
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.float x)
      , ("y", E.float y)
      , ("size", E.float size)
      ]

encodeFly : Fly -> E.Value
encodeFly {position} =
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.float x)
      , ("y", E.float y)
      ]

encodeGame : Model -> E.Value
encodeGame model =
  E.object
    [ ("height", E.int model.height)
    , ("width", E.int model.width)
    , ("score", E.int model.score)
    , ("hero", encodeHero model.hero)
    , ("flies", E.list encodeFly model.flies)
    ]

view : Model -> Html Msg
view model =
  node "game-canvas" [height model.height, width model.width] []