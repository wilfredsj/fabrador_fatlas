module ElmConstructor

open ElmRender

open OpenTK.Windowing.Desktop
open OpenTK.Mathematics

let createElmWindow (modelMaker : 'ModelInit -> 'Model) 
                    (callbackMaker : ((Vector3 [] * Vector3 [] * int [] * string) list -> unit) -> 'ModelInit)
                    (updater : 'Model -> PassedEvent<'Msg> -> 'Model) 
                    (queue : 'Msg list) = 
  
  let gws = GameWindowSettings()
  let nws = NativeWindowSettings()
  nws.Size <- Vector2i(600, 800)
  nws.Title <- "Sandbox"
  let window = new ElmLikeWindow<'Msg,'Model>(gws, nws, None, updater)
  let modelInit = callbackMaker window.changeVerticesTuples
  let model = modelMaker modelInit 
  
  let m' = List.fold (fun s m -> updater s (direct m)) model queue

  window.overrideState m'
  
  do window.Run()