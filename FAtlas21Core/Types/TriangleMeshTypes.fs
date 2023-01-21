namespace FAtlas

open CoordTypes

module TriangleMeshTypes =
  type CoordinateKeys = (char*int) list

  type TriangleFrame = { frameMap : Map<char, Cartesian>; scaleFactor : int }

  type KeyedPoint<'B> = { key : (char * int) list; datum : 'B }

  type SingleTriangle<'A> = { keys : char list; scale : int; points : 'A array array }
  
  let sideLength N = N+1
  let dim st =
    (Array.length st.points) - 1

  let makeEmptyTriangle<'A> keys N =
    let side = sideLength N
    { keys = keys; scale = N; points = Array.init side (fun i -> Array.empty<'A>)}

  let tmap f triangle = 
    { keys = triangle.keys; scale = triangle.scale; 
      points = 
        triangle.points 
        |> Array.map(Array.map f)}

  let tkmap f triangle = 
    { keys = triangle.keys; scale = triangle.scale; 
      points = 
        triangle.points 
        |> Array.map(Array.map(fun kp -> { key = kp.key; datum = f kp.datum}))} 

  type OrientedVertex = 
  | OV_A
  | OV_B
  | OV_C

  let ovFromInt = function
                  | 0 -> OV_A
                  | 1 -> OV_B
                  | 2 -> OV_C
                  | _ -> failwith "Logic Error"
        
  type OrientedEdge =
  | OE_AB
  | OE_BC
  | OE_CA
  | OE_BA
  | OE_CB
  | OE_AC
  
  // The 'U' in URL stands for Unique
  // These are not-unique unless there's some normalization in the construction
  // ... there's actually a function `normalizeElement` which does this.
  // ... which is used by `getVertexNeighbours` 
  // Whether unique or not, can be thought of as O(1) pointer to a vertex
  type VertexUrl = { t : int; i : int; j : int }
  let vtxStr vtx = sprintf "(%i, %i)[%i]" vtx.i vtx.j vtx.t
  type EdgeData = { duplicateEdges : (int*OrientedEdge) list; canonical : (int*OrientedEdge)}  
  type VertexData = { facesTouched : (int*OrientedVertex) list; canonical : (int*OrientedVertex) }
  let makeEdge edges =
    let canon = edges |> List.sortBy(fun (a,b) -> a) |> List.head
    { duplicateEdges = edges; canonical = canon }

  type TriangleSet<'A> = { 
    frame : TriangleFrame; 
    triangles : SingleTriangle<'A> [];
    trianglesByVertex : Map<char, VertexData>
    trianglesByEdge : Map<char*char, EdgeData>
    }

  
  // points
  // "A" [0][0]    ->        -> "C" [0][N]
  //     [1][0]                [1][N-1]
  //  v
  //     [N-1][0] [N-1][1]
  // "B" [N][0]
  
  // SingleTriangle -> int -> int -> TrianglePoint
  let extractPoint_ij st i j =
    st.points.[i].[j] 

  //    or if in diagonal slices 
  // "A" "0,0" ////  <- "C" = "N,N"
  //           ///
  //           //
  //           /
  //           ^-- "B" = "N,0"
  // SingleTriangle -> int -> int -> TrianglePoint
  let extractPoint_rt st radius index =
    st.points.[radius-index].[index] 

  // SingleTriangle -> (TrianglePoint x 3)
  let extractABC tr =
    let xa = tr.points.[0].[0]
    let xb = tr.points.[tr.points.Length - 1].[0]
    let xc = tr.points.[0].[tr.points.Length - 1]
    (xa, xb, xc)

  let makeABC (ak,bk,ck) (a,b,c) =
    let mapper i j = 
      match (i,j) with
      | (0, 0) -> a
      | (0, 1) -> c
      | (1, 0) -> b
      | (_, _) -> failwith "logic error"
    { keys = [ak;bk;ck]; scale = 0; points = Array.init 2 (fun i -> Array.init (2 - i) (mapper i)) }
    

    
    
  // TriangleFrame -> SingleTriangle -> (Cartesian x 3)
  let extractContext frame tr =
    let map = frame.frameMap
    match tr.keys |> List.map(fun c -> map |> Map.find(c)) with
    | a :: b :: c :: _ -> (a,b,c)
    | _ -> failwith "Incorrect number of keys"
      
  // SingleTriangle -> (char x 3)
  let extractKey tr =
    match tr.keys with
    | a :: b :: c :: _ -> (a,b,c)
    | _ -> failwith "Incorrect number of keys"

  let extractCorner N tr ch =
    tr.keys 
    |> List.findIndex(fun c -> c = ch)
    |> function 
       | 0 -> (0, 0)
       | 1 -> (0, N)
       | 2 -> (N, 0)
       | _ -> failwith "Logic error"
  
  let triangleSet<'A> frame (triangles : SingleTriangle<'A>[]) =
    let lookupInMapOrAdd key value map =
      match Map.tryFind key map with
      | Some(is) -> Map.add key (value :: is) map
      | None -> Map.add key [value] map
    let addVertexCache map (i,t) =
      t.keys
      |> List.indexed
      |> List.fold (fun m (j,c) -> lookupInMapOrAdd c (i,ovFromInt j) m) map

    let normalizeVertexCache =
      Map.map(fun key faceIndices -> 
        let i = faceIndices |> List.minBy(fst)
        { facesTouched = faceIndices; canonical = i })
    let addEdgeCache map (i,t) =
      let (a,b,c) = extractKey t
      let elts = [
        ((a,b), (i,OE_AB));
        ((b,a), (i,OE_BA));
        ((b,c), (i,OE_BC));
        ((c,b), (i,OE_CB));
        ((c,a), (i,OE_CA));
        ((a,c), (i,OE_AC))
      ]
      List.fold (fun m (k,v) -> lookupInMapOrAdd k v m) map elts
    let ti = triangles |> Array.indexed |> List.ofArray
    let vertexCache = 
      ti
      |> List.fold addVertexCache Map.empty
      |> normalizeVertexCache
    let edgeCache = 
      ti
      |> List.fold addEdgeCache Map.empty
      |> Map.map (fun k v -> makeEdge v)
    { frame = frame; 
      triangles = triangles; 
      trianglesByVertex = vertexCache;
      trianglesByEdge = edgeCache }
    
  let makeSet keys triangles =
    match triangles with
    | crib :: _ ->
      let keyMap = keys |> Map.ofList
      let scale = crib.scale
      let frame = { frameMap = keyMap; scaleFactor = scale }
      triangleSet frame (triangles |> Array.ofList)
    | _ -> failwith "Empty triangle list in TriangleTypes.makeSet"


  type VertexConverters = { nplusOne : int; vertices1dTo2d : Map<int,int*int>; vertices2dTo1d : Map<int*int,int> }