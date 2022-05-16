module Main exposing (..)

import Browser
import Random
import Array exposing (Array)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import Array exposing (initialize)
import Array exposing (repeat)

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , grid : List String
  , snake : Array Int
  , apple : Int
  , currentMove : Key
  , length : Int
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time (initGrid [] 0) (initialize 4 (\n -> n+160)) 150 Space 40
  |> Update.none

initGrid : List String -> Int -> List String
initGrid grid n = 
  if (n /= 400) then
    grid
  else
    initGrid grid n

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | CollisionAppleSnake
  | NewAppleRandomPosition Int


snakeHead: Array Int -> Int
snakeHead snake = 
  case (Array.get 0 snake) of 
    Just a-> a
    Nothing -> 0

snakeMove: Array Int -> Int -> Array Int
snakeMove snake nextStep =
  let 
    newHeadPosition = Array.repeat 1 nextStep
    snakeBody = Array.slice 0 ( ( Array.length snake ) - 1 ) snake
  in
  Array.append newHeadPosition snakeBody


addSnakePart : Array Int -> Array Int
addSnakePart snake =
  let 
    newPartPosition = Array.get ( (Array.length snake) - 1 ) snake 
    ab = Debug.log "" newPartPosition
    bbb = Debug.log "" snake

  in
  case newPartPosition of 
  Nothing -> snake
  Just a -> (Array.push a snake)

collisionApple : Model -> Model
collisionApple ({ apple, snake } as model) =
  if isCollisionApple model then
    {model | snake = addSnakePart snake }
  else 
    {model | snake = model.snake}

isCollisionApple : Model -> Bool
isCollisionApple ({ apple, snake } as model) =
  if (snakeHead snake == apple) then
    True
  else 
    False


{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
updateSquare : Model -> Key -> Model
updateSquare model choice =
  let snakeH = (snakeHead model.snake) in
  if (isGoodStep model choice (position model snakeH)) then
    case choice of
      ArrowRight -> 
          snakeH + 1
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowLeft ->
        if (modBy model.length snakeH == 0) then 
          snakeH
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
        else
          snakeH- 1
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowDown -> 
          snakeH + model.length
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowUp ->
          snakeH  - model.length
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      Space ->
          snakeH
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
  else
    snakeH
    |> modBy (model.length*model.length)
    |> snakeMove model.snake
    |> Setters.setColoredSquareIn model

position : Model -> Int -> (Int, Int)
position model i = 
  ((i // model.length), modBy model.length i)

isGoodStep : Model -> Key -> (Int, Int)-> Bool
isGoodStep model key pos= 
  case key of
      ArrowRight ->  let (a, b) = pos in b < 39
      ArrowLeft ->  let (a, b) = pos in b > 0
      ArrowUp -> let (a, b) = pos in a > 0
      ArrowDown ->  let (a, b) = pos in a < 39
      Space -> True
      

updateCurrentMove : Model -> Key -> Model
updateCurrentMove model currentMove =
  case currentMove of 
    Space -> { model | currentMove = currentMove }
    ArrowLeft -> if (model.currentMove /= ArrowRight) then { model | currentMove = currentMove } else model
    ArrowRight -> if (model.currentMove /= ArrowLeft) then { model | currentMove = currentMove } else model
    ArrowUp -> if (model.currentMove /= ArrowDown) then { model | currentMove = currentMove } else model
    ArrowDown -> if (model.currentMove /= ArrowUp) then { model | currentMove = currentMove } else model

isSnakePart: Int -> List Int -> Bool
isSnakePart elem snake =
  case snake of
      [] -> False
      head::rest ->
        if (elem == head) then
          True
        else
          isSnakePart elem rest

randomPosition : Cmd Msg
randomPosition =
    let r = Random.int 1 400 in
    Random.generate NewAppleRandomPosition r

toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> 
      update ToggleGameLoop model
    ArrowLeft -> 
      updateCurrentMove model key
      |> Update.none
    ArrowRight -> 
      updateCurrentMove model ArrowRight
      |> Update.none
    ArrowUp -> 
      updateCurrentMove model ArrowUp
      |> Update.none
    ArrowDown -> 
      updateCurrentMove model ArrowDown
      |> Update.none

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let 
    time_ = Time.posixToMillis time
  in
  if time_ - model.lastUpdate >= 150 then
    updateSquare model model.currentMove
    |> collisionApple 
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model
    CollisionAppleSnake -> (model,randomPosition)
    NewAppleRandomPosition pos ->
      if (isCollisionApple model) then
        (model,randomPosition) 
      else 
        {model | apple = pos } |> Update.none


isSnakeCell : List Int -> Int -> Bool
isSnakeCell snake index = 
  case snake of 
    [] -> False
    head::rest ->
      if (head == index) then
        True
      else 
        isSnakeCell rest index


{-| Manage all your view functions here. -}
cell : Int -> Model -> Html msg 
cell index ({ snake , apple } as model) =
  let 
    snakeH = snakeHead snake
    class = if apple == index then 
                "cell activeApple"
              else 
                  if isSnakeCell (Array.toList snake) index then
                    "cell active"
                  else 
                    "cell"
    
  in

  Html.div [ Attributes.class class ] []

generateCells : Model -> Int -> List (Html msg)
generateCells model n =
  case n of
    0 -> []
    _ -> generateCells model (n-1) ++ [cell (n-1) model]

movingSquare : Model -> Html msg
movingSquare coloredSquare =
  Html.div [ Attributes.class "grid" ]
   (generateCells coloredSquare (40*40))

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [
    explanations model
    , movingSquare model

    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
  
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    
    }
