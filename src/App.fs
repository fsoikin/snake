module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.React
open Fable.Helpers.React.Props

importAll "../sass/main.sass"

type Square = { x: int; y: int }

type Direction = Down | Up | Left | Right

type Cake = { position: Square; growth: int }

type Model =
    {
        head: Square
        headDirection: Direction
        tail: Square list
        cakes: Cake list
        growth: int
        gameOver: bool
        tickLengthTimes10: int
        log: string list
    }

type Message =
    | ChangeDirection of Direction
    | Tick
    | Nop

let fieldWidth = 80
let fieldHeight = 80
let squareWidth = 10
let squareHeight = 10
let maxCakeGrowth = 5

let minX, maxX, minY, maxY = -fieldWidth/2, fieldWidth/2-1, -fieldHeight/2+1, fieldHeight/2

let mkTick len =
    Cmd.ofAsync (fun _ -> Async.Sleep len) () (fun _ -> Tick) (fun _ -> Tick)

let catchKeys() =
    Cmd.ofSub <| fun d ->
        document.addEventListener_keydown(fun e ->
            match e.key with
            | "ArrowUp" -> d <| ChangeDirection Up
            | "ArrowDown" -> d <| ChangeDirection Down
            | "ArrowLeft" -> d <| ChangeDirection Left
            | "ArrowRight" -> d <| ChangeDirection Right
            | _ -> d Nop

            box 0)

let coerce s =
    let r = {
        x =
            if s.x < minX then maxX + 1 - (minX-s.x)
            elif s.x > maxX then minX - 1 + (s.x-maxX)
            else s.x
        y =
            if s.y < minY then maxY + 1 - (minY-s.y)
            elif s.y > maxY then minY - 1 + (s.y-maxY)
            else s.y
    }
    //console.log <| sprintf "%A -> %A" s r
    r

let mkCake seed =
    let rnd = new System.Random( seed * System.Environment.TickCount )
    {
        position = { x = rnd.Next(minX, maxX); y = rnd.Next(minY, maxY) }
        growth = rnd.Next(1, maxCakeGrowth)
    }

let init _ =
    let initState = {
        head = { x = 0; y = 0 }
        headDirection = Down
        tail = [for y in 1..10 -> { x = 0; y = y }]
        cakes = [for i in 1..3 -> mkCake i]
        growth = 0
        gameOver = false
        tickLengthTimes10 = 5000
        log = []
    }
    initState, Cmd.batch [mkTick 500; catchKeys()]

let move s d =
    match d with
    | Down -> { s with y = s.y - 1 }
    | Up -> { s with y = s.y + 1 }
    | Right -> { s with x = s.x + 1 }
    | Left -> { s with x = s.x - 1 }

let onTail s tail = List.contains s tail

let cakePositionIs pos cake = cake.position = pos

let isSome = function | Some _ -> true | None -> false

let log s mdl = if s = "" then mdl else { mdl with log = List.truncate 25 (s :: mdl.log) }

let update msg mdl =
    match msg with
    | ChangeDirection d ->
        let newHead = move mdl.head d
        if onTail newHead (List.truncate 1 mdl.tail) then
            mdl, []
        else
            { mdl with headDirection = d }, []
    | Tick when mdl.gameOver ->
        mdl, []
    | Tick ->
        let newHead = move mdl.head mdl.headDirection
        let newTailLength = if mdl.growth > 0 then List.length mdl.tail + 1 else List.length mdl.tail
        let newTail = List.take newTailLength (mdl.head :: mdl.tail)

        let eatenCake = List.tryFind (cakePositionIs <| coerce newHead) mdl.cakes
        let addedGrowth =
            match eatenCake with
            | Some c -> c.growth
            | None -> 0
        let newCakes =
            match eatenCake with
            | Some c -> mkCake 1 :: List.filter (fun ck -> ck <> c) mdl.cakes
            | None -> mdl.cakes
        let cakeLog =
            match eatenCake with
            | Some c -> sprintf "Ate a cake size = %d" c.growth
            | None -> ""

        if onTail newHead newTail then
            if mdl.gameOver then
                mdl, []
            else
                log "Game over" { mdl with gameOver = true }, []
        else
            log cakeLog
                { mdl with
                    head = coerce newHead
                    tail = List.map coerce newTail
                    cakes = newCakes
                    growth = max 0 (mdl.growth - 1) + addedGrowth
                    tickLengthTimes10 = max 300 (mdl.tickLengthTimes10 - 50*addedGrowth)
                }
            , mkTick (mdl.tickLengthTimes10/10)

type Point = { pointX: int; pointY: int }
let toScreenCoords x y = {
    pointX = (fieldWidth/2 + x)*squareWidth
    pointY = (fieldHeight/2 - y)*squareHeight
}

let view mdl dispatch =
    let tailColor = if mdl.gameOver then "red" else "black"
    let headColor = "blue"
    let cakeColor growth = sprintf "rgb(0,%d,0)" (255*growth/maxCakeGrowth)

    let square squareColor {x = x; y = y} =
        let p = toScreenCoords x y
        div [Style [
                Position "absolute"
                Props.Left p.pointX
                Props.Top p.pointY
                CSSProp.Width (squareWidth-2)
                CSSProp.Height (squareHeight-2)
                BackgroundColor squareColor
            ]]
            []

    let statRow name value =
            div [ClassName "row"] [
                span [ClassName "name"] [str (name + ": ")]
                span [ClassName "value"] [str value]
            ]

    div [ClassName "container"] [
        div
            [ Style [CSSProp.Width (fieldWidth*squareWidth); CSSProp.Height (fieldHeight*squareHeight)]
              ClassName "field"
            ]
            (
                (square headColor mdl.head :: List.map (square tailColor) mdl.tail)
                @ (mdl.cakes |> List.map (fun c -> square (cakeColor c.growth) c.position))
            )
        div [ClassName "sidebar"] [
            div [ClassName "stats"] [
                statRow "Length" <| string (List.length mdl.tail + 1)
                statRow "Growth" <| string mdl.growth
                statRow "Speed" <| (string (mdl.tickLengthTimes10/10) + "ms")
            ]
            div [ClassName "log-container"] [
                div [ClassName "log"] [
                    for line in mdl.log -> div [ClassName "line"] [str line]
                ]
            ]
        ]
    ]


open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
