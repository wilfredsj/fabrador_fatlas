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
open FAtlas.AtlasStateTypes

open ElmConstructor

let testData = hexAsPrimitive [{ row = 0; dicol = 0; z = 1.0f}; { row = 0; dicol = 1; z= 0.8f}; { row = 1; dicol = 0; z= 1.2f}]

let atlasCallback updateVertices = { makeVertex = Vector3; makeColour = Vector3; onUpdateCallback = updateVertices }

let adaptedMsg s m =
  match m with
  | DirectMessage m1 -> updateModel s m1
  | KeyDown ch -> onkeyPress s ch
  | EventNoOp -> s

[<EntryPoint>]
let main argv =
  let z = cart 0.0 -1.0 1.0
  let y = coordFromCart z
  let x = cartFromSphere y

  let g = createIcosahedron ()
  let gws = GameWindowSettings()

  let x1 = PrimitiveType.Parse(typedefof<PrimitiveType>, "Lines") :?> PrimitiveType

  let initScript = [NoOp; Divide 4; ClusterInit None; ClusterIterate 5000] 
  createElmWindow initState atlasCallback adaptedMsg initScript

  // Fixed - 1: Why fail when ClusterIterate 50?
  // Fixed - 2: If vertex added to 1 cluster, needs to be removed from border of other clusters
  // Fixed - 3: If x-times in border, auto-add?
  // Fixed - 4: Which clusters touch other clusters?
  // Fixed - 5: Terminal condition ?
  // Fixed - 6: Keyboard callback to Atlas
  // Fixed - 7: Decouple Render modesl

  // TODO:
  // AETHER-8:  assign base heights to clusters
  //       -9:  parameterise boundary of cluster (able to determine (r,th') coordinates
  //       -10: assign momentum to cluster
  //       -11: stress function based on momentum
  //       -12: render mode for stress
  //       -13: render mode for base height

  1