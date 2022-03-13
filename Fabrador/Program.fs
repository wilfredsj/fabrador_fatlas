// Learn more about F# at http://fsharp.org

open System

open OpenTK.Windowing.Desktop
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open ElmRender
open HexaHelpers

open FAtlas.CoordFunctions
open FAtlas.CoordTypes
open FAtlas.TectonicFunctions

open FAtlas.Interface

let testData = hexAsPrimitive [{ row = 0; dicol = 0; z = 1.0f}; { row = 0; dicol = 1; z= 0.8f}; { row = 1; dicol = 0; z= 1.2f}]

let atlasCallback (w : ElmLikeWindow) = { makeVertex = Vector3; makeColour = Vector3; onUpdateCallback = w.changeVerticesTuples }


type ElmHandler = {
  state : AtlasState<Vector3, Vector3>;
  messages : Message list
}

[<EntryPoint>]
let main argv =
  let z = cart 0.0 -1.0 1.0
  let y = coordFromCart z
  let x = cartFromSphere y

  let g = createIcosahedron ()
  let gws = GameWindowSettings()

  let x1 = PrimitiveType.Parse(typedefof<PrimitiveType>, "Lines") :?> PrimitiveType

  let nws = NativeWindowSettings()
  nws.Size <- Vector2i(600, 800)
  nws.Title <- "Sandbox"
  let window = new ElmLikeWindow(gws, nws, None)
  window.changeVertices testData 
  let callback = atlasCallback window
  let atlas = initState callback

  let initScript = [NoOp; Divide 4; ClusterInit None; ClusterIterate 5000]
  let m' = updateModelWithScript atlas initScript
  do window.Run()

  // Fixed :  Why fail when ClusterIterate 50?
  // Fixed : If vertex added to 1 cluster, needs to be removed from border of other clusters
  // Fixed : If x-times in border, auto-add?
  // Fixed : Which clusters touch other clusters?
  // Fixed : Terminal condition ?

  // TODO:
  // Keyboard callback to Atlas
  // Decouple Render modesl
  

  1