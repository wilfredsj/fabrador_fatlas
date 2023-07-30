namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open ConsoleTypes

module AtlasStateTypes =

  type HeightBiasType =
  | HB_None
  | HB_Flat
  | HB_Linear
  | HB_Stressed

  type ColourScheme =
  | GrayScale
  | TectonicColours of int Option
  | TectonicLocalCoordColours of int Option
  | TectonicStressColours of int Option
  | TectonicHeightBiasColours of (int Option*HeightBiasType*HeightBiasType)
  | TectonicHeightBias of (int Option)
  | HeightBestEffort of bool

  type ClusterViewArgs = { colours : ColourScheme; wireframeConnections : bool }

  type BorderViewMode =
  | JustBorder
  | LocalCoordinates

  type RenderMode = 
  | BasicCoordinate
  | IcosaView of ColourScheme  
  | IcosaViewFiltered of ColourScheme*int
  | ClusterView of ClusterViewArgs
  | BorderView of BorderViewMode*(int Option)
  | MercatorView
  | GeoMeshView of ColourScheme*(int Option)*bool*bool

  let sprintRenderMode =
    function
    | BasicCoordinate -> "BasicCoordinate"
    | IcosaView cs -> sprintf "IcosaView %A" cs
    | IcosaViewFiltered (cs,i)-> sprintf "IcosaViewFiltered %A %i" cs i
    | ClusterView cs -> sprintf "ClusterView %A" cs
    | BorderView (cs,i) -> sprintf "BorderView %A %A" cs i
    | GeoMeshView (cs,iOpt,b1,b2)-> sprintf "GeoMeshView %A %A %b %b" cs iOpt b1 b2
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

  type ConsoleAction =
  | Print
  | Stats
  | Details

  type ConsoleTarget = 
  | State
  | GeoMesh
  | Tectonics
  | Cluster

  let defaultTarget action =
    match action with
    | Print -> State
    | Stats -> State
    | Details -> GeoMesh

  type ConsoleCommandTyped = 
    { action : ConsoleAction; target : ConsoleTarget; args : string list }

  let consoleCommand a tOpt args = 
    { action = a; target = tOpt |> Option.defaultValue (defaultTarget a); args = args }

  type Message =
  | NoOp
  | Restart
  | ReSeed of int
  | Divide of int
  | NewRenderMode of RenderMode
  | ConsoleCommand of ConsoleCommandTyped
  | UnknownCommand of string
  | UIInstruction of UIAction
  | ClusterInit of int option
  | ClusterIterate of int
  | AssignTectonics
  | InitGeoMesh of bool

  type ModelState =
  | Init
  | IcosaDivision of TriangleSet<KeyedPoint<Coordinate>>
  | ClusterAssignment of ClusterAssigmentState<(char*int) list>*VertexConverters
  | ClusterFinished of CompleteClusterAssignment<(char*int) list>
  | TectonicAssigned of TectonicData<(char*int) list>
  | GeoDivision of GeoDivisionState<(char*int) list>

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
  type AtlasCache = 
    { 
      triConverters : Map<int, VertexConverters>; 
      hexConverters : Map<int, HexVertexConverters>; 
      hexConvertersBD : Map<int, HexVertexConverters_BadDual> 
    }
  let emptyCache = { triConverters = Map.empty; hexConverters = Map.empty; hexConvertersBD = Map.empty }
  type AtlasConsoleCache =
    {
      lastConsoleCommand : ConsoleCommandTyped Option
      cachedArgs : CachedArg list
    }
  let emptyConsoleCache = { lastConsoleCommand = None; cachedArgs = List.empty }
  type AtlasState<'V,'C> = 
    { 
      render : RenderMode; 
      model : ModelState; 
      callbacks : AtlasCallbacks<'V,'C>; 
      renderCache : AtlasCache
      consoleCache : AtlasConsoleCache
    }