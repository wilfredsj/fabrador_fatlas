namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes

module AtlasStateTypes =

  type ColourScheme =
  | GrayScale
  | TectonicColours

  type ClusterViewArgs = { colours : ColourScheme; wireframeConnections : bool }

  type RenderMode = 
  | BasicCoordinate
  | IcosaView of ColourScheme  
  | ClusterView of ClusterViewArgs
  | MercatorView

  type UIAction =
  | ForceEuclidian
  | ForceMercator

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
  type UIUnitCallbacks = { forceEuclidian : unit -> unit; forceMercator : unit -> unit }
  type AtlasCallbacks<'V,'C> = { 
    makeVertex : A3V -> 'V; 
    makeColour : A3V -> 'C; 
    onUpdateCallback : (('V []*'C[]*int[]*string) list) -> unit;
    uiCallbacks : UIUnitCallbacks}
  type AtlasCache = { vertexConverters : Map<int, VertexConverters> }
  let emptyCache = { vertexConverters = Map.empty }
  type AtlasState<'V,'C> = { render : RenderMode; model : ModelState; callbacks : AtlasCallbacks<'V,'C>; renderCache : AtlasCache}