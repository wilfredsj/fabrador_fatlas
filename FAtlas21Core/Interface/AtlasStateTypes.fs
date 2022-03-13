namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes

module AtlasStateTypes =

  type RenderMode = 
  | BasicCoordinate

  type Message =
  | NoOp
  | Divide of int
  | NewRenderMode of RenderMode
  | ClusterInit of int option
  | ClusterIterate of int

  type ModelState =
  | Init
  | IcosaDivision of TriangleSet<KeyedPoint<Coordinate>>
  | ClusterAssignment of ClusterAssigmentState<(char*int) list>*VertexConverters
  | ClusterFinished of CompleteClusterAssignment<(char*int) list>