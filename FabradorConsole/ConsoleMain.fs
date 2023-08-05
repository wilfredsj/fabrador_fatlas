open System
open FAtlas.Interface
open FAtlas.AtlasStateTypes

let createFatlasInstance () = 
  let callbacks = { 
    makeVertex = id
    makeColour = id
    onUpdateCallback = fun x -> ()
    uiCallbackOpt = None
  }
  initState callbacks

let sendMessageFromConsoleToModel model () =
  printf ">>"
  let msgText = Console.ReadLine()
  let model' = onConsoleInput model msgText
  model'

let rec mainLoop model =
  let model' = sendMessageFromConsoleToModel model ()
  mainLoop model'

[<EntryPoint>]
let main argv =
  mainLoop (createFatlasInstance ())
  0 // return an integer exit code
