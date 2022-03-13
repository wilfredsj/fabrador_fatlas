namespace FAtlas

open CoordTypes
open TriangleMeshTypes

open TectonicFunctions
open TriangleMeshToRender
open TriangleMeshFunctions
open CoordFunctions
open ColourTypes
open TectonicTypes
open TectonicViewFunctions
open AtlasIO

module Interface =
  let rng = System.Random(1338)

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

  type A3V = float32*float32*float32

  type AtlasCallbacks<'V,'C> = { makeVertex : A3V -> 'V; makeColour : A3V -> 'C; onUpdateCallback : (('V []*'C[]*int[]*string) list) -> unit }

  type AtlasCache = { vertexConverters : Map<int, VertexConverters> }
  let emptyCache = { vertexConverters = Map.empty }

  let getVertexConverter ac np1 =
    match Map.tryFind np1 ac.vertexConverters with
    | Some(vc) -> (vc, None)
    | None ->
      let newElt = vertexConverters np1
      let vcs' = Map.add np1 newElt ac.vertexConverters
      (newElt, Some(vcs'))

  type AtlasState<'V,'C> = { render : RenderMode; model : ModelState; callbacks : AtlasCallbacks<'V,'C>; renderCache : AtlasCache}

  let initState callbacks = { render = BasicCoordinate; model = Init; callbacks = callbacks; renderCache = emptyCache }

  let getNp1 t =
    t.points.Length

  let getNp1TS ts = 
    getNp1 ts.triangles.[0]

  let drawIcosaSection r colourer (state : AtlasState<'V,'C>) (ts : TriangleSet<'A>) rc (i,t) =
    let toSimpleCart ij (coord : KeyedPoint<Coordinate>) = cartFromSphereWithRadius r coord.datum
    
    // let colourer ij k = (0.6f,0.2f,0.5f) |> state.callbacks.makeColour 
    
    let cacheUsed = Option.defaultValue state.renderCache rc
    let (converters, cacheOpt) = getVertexConverter cacheUsed (getNp1 t)
    let rc' = 
      match cacheOpt with
      | Some(c) -> { vertexConverters = c } |> Some
      | None -> rc
    let abc = asArrays state.callbacks.makeVertex toSimpleCart (colourer state.callbacks.makeColour i) converters ts.frame t
    (abc, rc')
  
  let concat3 strGlType (tuples : ('a[]*'b[]*int [])[]) =
    let (c1,c2,c3) = tuples.[0]
    let n1 = tuples |> Array.fold(fun acc (a,_,_) -> Array.length(a) + acc) 0
    let n2 = tuples |> Array.fold(fun acc (_,_,a) -> Array.length(a) + acc) 0
    let a = Array.create n1 c1.[0]
    let b = Array.create n1 c2.[0]
    let c = Array.create n2 c3.[0]
    tuples 
    |> Array.fold(fun (n1a, n2a) (al,bl,cl) -> 
      al |> Array.indexed |> Array.iter(fun (i,ai) -> a.[n1a+i] <- ai)
      bl |> Array.indexed |> Array.iter(fun (i,bi) -> b.[n1a+i] <- bi)
      cl |> Array.indexed |> Array.iter(fun (i,ci) -> c.[n2a+i] <- ci + n1a)
      (n1a + Array.length al, n2a + Array.length cl)) (0,0)
    |> ignore
    (a,b,c,strGlType)

  let uniformHue max mkColour i ij k = rangeToFullSat 0.0 max (float i) |> makeRGB|> mkColour
  let uniformHue2 sat value max mkColour i ij k = rangeWithSatValue sat value 0.0 max (float i) |> makeRGB|> mkColour
  let grayscale mkColour i ij k = ((i * 79) % 71) |> float32 |> fun y -> (y / 71.0f) * 0.4f + 0.1f |> fun z -> (z,z,z) |> mkColour
  let allGray mkColour i ij k = 0.7f |> fun z -> (z,z,z) |> mkColour

  let tectonicColours (clusterState : ClusterAssigmentState<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.unfinishedClusters |> Array.length |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.lookupFromKey
    |> function 
       | None -> grayscale mkColour i ij k
       | Some(c) -> uniformHue nc mkColour c ij k

  let tectonicColours2 (clusterState : CompleteClusterAssignment<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.allClusters |> Array.length |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.lookupFromKey
    |> function 
      | None -> grayscale mkColour i ij k
      | Some(c) -> uniformHue2 0.6f 0.6f nc mkColour c ij k

  let solidViewIcosaSection colourer state ts =
    let (elts, newCache) =
      ts.triangles 
      |> Array.indexed
      |> Array.mapFold (drawIcosaSection 1.0 colourer state ts) None
    state.callbacks.onUpdateCallback [concat3 "Triangles" elts]
    newCache 

  let solidAndWireViewIcosaSection wireColourer state ccs =
    let ts = ccs.meshData
    let (wires'', newCache) =
      ts.triangles 
      |> Array.indexed
      |> Array.mapFold (drawIcosaSection 1.0 wireColourer state ts) None

   // let (w1,w2,w3) = viewClustersAsNetwork state.callbacks.makeVertex state.callbacks.makeColour ccs
    //let (w1,w2,w3) = viewClusterToBorderNodes state.callbacks.makeVertex state.callbacks.makeColour (Some 1) ccs
    let (w1,w2,w3) = viewClusterToBorderNodes state.callbacks.makeVertex state.callbacks.makeColour None ccs
    let lineSection = (w1,w2,w3,"Lines")
    let (solid, _) =
      ts.triangles 
      |> Array.indexed
      //|> Array.mapFold (drawIcosaSection 0.6 allGray state ts) None
      |> Array.mapFold (drawIcosaSection 0.6 (tectonicColours2 ccs) state ts) None
    
    let allElts =
      [concat3 "Triangles" solid; lineSection]

    state.callbacks.onUpdateCallback allElts
    newCache 
    

  let updateView state = 
    match state.model with
    | IcosaDivision ts -> 
      solidViewIcosaSection grayscale state ts
    | ClusterAssignment (cas, vc) -> 
      solidViewIcosaSection (tectonicColours cas) state cas.meshData
    | ClusterFinished ccs -> 
      solidAndWireViewIcosaSection (tectonicColours2 ccs) state ccs
    | _ -> None

  let maybeUpdateCacheState state renderCacheOpt = 
    match renderCacheOpt with
    | Some(rc) -> { state with renderCache = rc }
    | None -> state

  let intoClusterState state ts nOpt = 
    let n = Option.defaultValue (defaultNumClusters ts) nOpt
    let (vc, _) = getVertexConverter state.renderCache (getNp1TS ts)
    let init = initializePartitions rng vc.vertices1dTo2d ts n
    { state with model = ClusterAssignment (init, vc)}

  let divideClusterState state (cs : ClusterAssigmentState<(char*int) list>) vc n =
    let ncs =
      [1 .. n]
      |> List.fold (fun s _ -> expandOneCluster rng cs.meshData s) cs
    if finishedAssigment ncs then
      let ncs' = finalize ncs
      { state with model = ClusterFinished ncs' }
    else
      { state with model = ClusterAssignment (ncs, vc)}

  let updateModel state message =
    if state.model = Init then
      let data = createIcosahedron()
      let newModel = IcosaDivision data
      let ns = { state with model = newModel }
      updateView ns
      |> maybeUpdateCacheState ns
    else
      match message with
      | Divide i ->
        let newModel =
          match state.model with
          | IcosaDivision ts -> 
            [1 .. i] 
            |> List.fold (fun t i -> divideIcosahedron t) ts
            |> IcosaDivision
          | x -> x
        let ns = { state with model = newModel }
        updateView ns
        |> maybeUpdateCacheState ns
      | ClusterInit nOpt ->
        match state.model with
        | IcosaDivision ts -> 
          let ns = intoClusterState state ts nOpt
          updateView ns
          |> maybeUpdateCacheState ns
        | _ -> state
      | ClusterIterate n ->
        match state.model with
        | ClusterAssignment(cs, vc) ->
          let ns = divideClusterState state cs vc n
          updateView ns
          |> maybeUpdateCacheState ns
        | _ -> state
      | _ -> state


  let updateModelWithScript state = List.fold updateModel state

  let mutable keypress = { chars = "" }

  let onkeyPress ch = 
    let (kp', msg) = addToMessage keypress ch
    keypress <- kp'
    updateModel 

