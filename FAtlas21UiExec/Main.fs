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

let rotateConv = 
  function
  | Rotate_X x -> FRAx_X x
  | Rotate_Y x -> FRAx_Y x
  | Rotate_Z x -> FRAx_Z x
  | Rotate_Stop -> FRAx_Stop
  

let atlasCallback updateVertices eucl merc rot = 
  let uiCb = { 
    forceEuclidian = eucl; 
    forceMercator = merc 
    forceRotation = rotateConv >> rot
    }
  { makeVertex = Vector3; makeColour = Vector3; onUpdateCallback = updateVertices; uiCallbackOpt = Some uiCb}

let adaptedMsg s m =
  match m with
  | DirectMessage m1 -> updateModel s m1
  | KeyDown ch -> onkeyPress s ch
  | KeyEnter -> onEnterPress s
  | EventNoOp -> s

[<EntryPoint>]
let main argv =
  let z = cart 0.0 -1.0 1.0
  let y = coordFromCart z
  let x = cartFromSphere y

  let g = createIcosahedron ()
  let gws = GameWindowSettings()

  let x1 = PrimitiveType.Parse(typedefof<PrimitiveType>, "Lines") :?> PrimitiveType

  let initScript_ = [NoOp; Divide 4; ClusterInit None; ClusterIterate 5000] 
  let initScript = [NoOp; Divide 4; ClusterInit None; ClusterIterate 5000; AssignTectonics; InitGeoMesh false; Divide 1] 
  //let initScript = [NoOp; Divide 3; ClusterInit (Some 10); ClusterIterate 5000; AssignTectonics; InitGeoMesh false] 
  createElmWindow initState atlasCallback adaptedMsg initScript

  1