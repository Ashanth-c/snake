module Setters exposing (..)

import Functions exposing (flip)

setTime : b -> { a | time : b } -> { a | time : b }
setTime time record = { record | time = time }

setTimeIn : { a | time : b } -> b -> { a | time : b }
setTimeIn = flip setTime

setGameStarted : b -> { a | gameStarted : b } -> { a | gameStarted : b }
setGameStarted gameStarted record = { record | gameStarted = gameStarted }

setGameStartedIn : { a | gameStarted : b } -> b -> { a | gameStarted : b }
setGameStartedIn = flip setGameStarted

setApple : { a | gameStarted : b } -> b -> { a | gameStarted : b }
setApple = flip setGameStarted

setColoredSquare : b -> { a | snake : b } -> { a | snake : b }
setColoredSquare snake record = { record | snake = snake }

setColoredSquareIn : { a | snake : b } -> b -> { a | snake : b }
setColoredSquareIn = flip setColoredSquare

setLastUpdate : b -> { a | lastUpdate : b } -> { a | lastUpdate : b }
setLastUpdate lastUpdate record = { record | lastUpdate = lastUpdate }

setLastUpdateIn : { a | lastUpdate : b } -> b -> { a | lastUpdate : b }
setLastUpdateIn = flip setLastUpdate
