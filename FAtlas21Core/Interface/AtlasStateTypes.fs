﻿namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes

module AtlasStateTypes =

  type ColourScheme =
  | GrayScale
  | TectonicColours

  type RenderMode = 
  | BasicCoordinate
  | IcosaView of ColourScheme  
  | ClusterView of ColourScheme
  | MercatorView

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

  type A3V = float32*float32*float32
  type AtlasCallbacks<'V,'C> = { makeVertex : A3V -> 'V; makeColour : A3V -> 'C; onUpdateCallback : (('V []*'C[]*int[]*string) list) -> unit }
  type AtlasCache = { vertexConverters : Map<int, VertexConverters> }
  let emptyCache = { vertexConverters = Map.empty }
  type AtlasState<'V,'C> = { render : RenderMode; model : ModelState; callbacks : AtlasCallbacks<'V,'C>; renderCache : AtlasCache}