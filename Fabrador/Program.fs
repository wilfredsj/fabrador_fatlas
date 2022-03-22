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

  1