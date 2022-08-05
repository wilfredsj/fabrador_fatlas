namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes

module AtlasStateTypes =

  type ColourScheme =
  | GrayScale
  | TectonicColours of int Option
  | TectonicLocalCoordColours 

  type ClusterViewArgs = { colours : ColourScheme; wireframeConnections : bool }

  type BorderViewMode =
  | JustBorder
  | LocalCoordinates

  type RenderMode = 
  | BasicCoordinate
  | IcosaView of ColourScheme  
  | ClusterView of ClusterViewArgs
  | BorderView of BorderViewMode*(int Option)
  | MercatorView

  let sprintRenderMode =
    function
    | BasicCoordinate -> "BasicCoordinate"
    | IcosaView cs -> sprintf "IcosaView %A" cs
    | ClusterView cs -> sprintf "ClusterView %A" cs
    | BorderView (cs,i) -> sprintf "BorderView %A %A" cs i
    | MercatorView -> "MercatorView"

  type RenderRotationAction =
  | Rotate_X of bool
  | Rotate_Y of bool
  | Rotate_Z of bool
  | Rotate_Stop

  type UIAction =
  | ForceEuclidian
  | ForceMercator
  | ForceRotate of RenderRotationAction

  type Message =
  | NoOp
  | Restart
  | ReSeed of int
  | Divide of int
  | NewRenderMode of RenderMode
  | UIInstruction of UIAction
  | ClusterInit of int option
  | ClusterIterate of int

  type ModelState =
  | Init
  | IcosaDivision of TriangleSet<KeyedPoint<Coordinate>>
  | ClusterAssignment of ClusterAssigmentState<(char*int) list>*VertexConverters
  | ClusterFinished of CompleteClusterAssignment<(char*int) list>

  type A3V = float32*float32*float32
  type UIUnitCallbacks = { 
    forceEuclidian : unit -> unit; 
    forceMercator : unit -> unit;
    forceRotation : RenderRotationAction -> unit }
  type AtlasCallbacks<'V,'C> = { 
    makeVertex : A3V -> 'V; 
    makeColour : A3V -> 'C; 
    onUpdateCallback : (('V []*'C[]*int[]*string) list) -> unit;
    uiCallbackOpt : UIUnitCallbacks Option}
  type AtlasCache = { vertexConverters : Map<int, VertexConverters> }
  let emptyCache = { vertexConverters = Map.empty }
  type AtlasState<'V,'C> = { render : RenderMode; model : ModelState; callbacks : AtlasCallbacks<'V,'C>; renderCache : AtlasCache}