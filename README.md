# A simple websocket chat server

Based off of https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

## Build

    $ stack build

## Run

    $ stack exec blue-exe

## Connect using `wscat`

    $ wscat -c ws://localhost:9160
