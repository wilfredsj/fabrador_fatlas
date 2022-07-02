namespace FAtlas

open TriangleMeshTypes

open TectonicFunctions
open TectonicTypes
open AtlasIO
open AtlasStateTypes
open AtlasViewFunctions

module Interface =
  let mutable rng = System.Random(1338)


  let initState callbacks = { render = BasicCoordinate; model = Init; callbacks = callbacks; renderCache = emptyCache }

  let defaultRenderMode state = 
    match state.model with
    | Init 
    | IcosaDivision _ ->         IcosaView GrayScale
    | ClusterAssignment _ ->     IcosaView TectonicColours
    | ClusterFinished _ ->       ClusterView { colours = TectonicColours; wireframeConnections = true }
  
  let isSupportedRenderState state =
    match state.render with
    | MercatorView ->      true
    | IcosaView GrayScale -> true
    | IcosaView TectonicColours ->
      match state.model with
      | ClusterAssignment _
      | ClusterFinished _ -> true
      | _ -> false
    | ClusterView _ ->
      match state.model with
      | ClusterFinished _ -> true
      | _ -> false
    | _ -> false

  let getRenderMode state =
    if isSupportedRenderState state then
      state.render
    else
      defaultRenderMode state

  let extractTriangleSet s = 
    match s with
    | IcosaDivision t -> t
    | ClusterAssignment (cas, vc) -> cas.meshData
    | ClusterFinished cf -> cf.meshData
    | _ -> failwith <| sprintf "No triangles set for %A" s

  let extractClusterData s =
    match s with
    | ClusterAssignment (cas,_) -> renderCAS cas
    | ClusterFinished cf -> renderCCS cf
    | _ -> failwith <| sprintf "No cluster data for %A" s

  let extractCompleteClusterData s =
    match s with
    | ClusterFinished cf -> cf
    | _ -> failwith <| sprintf "No cluster data for %A" s

  let updateView state = 
    let renderModeUsed = getRenderMode state
    match renderModeUsed with
    | IcosaView cs ->
      match cs with 
      | GrayScale -> 
        solidViewIcosaSection (uniformHue 20.0) state (extractTriangleSet state.model)
      | TectonicColours ->
        solidViewIcosaSection (tectonicColours <| extractClusterData state.model) state (extractTriangleSet state.model)
    | ClusterView cs ->
      let colours = 
        match cs.colours with
        | TectonicColours ->
          (tectonicColours2 <| extractCompleteClusterData state.model)
        | _ ->
          failwith "Bad render combination"
      if cs.wireframeConnections then
        solidAndWireViewIcosaSection colours state (extractCompleteClusterData state.model)
      else
        solidViewIcosaSectionNoWires colours state (extractCompleteClusterData state.model)
    | MercatorView ->
        solidViewMercator state
        None
    | _ -> failwith "Unimplemented render mode"

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

  let blankState state =
    let data = createIcosahedron()
    let newModel = IcosaDivision data
    { state with model = newModel }

    

  let updateModel state message =
    if state.model = Init then
      let ns = blankState state
      updateView ns
      |> maybeUpdateCacheState ns
    else
      match message with
      | ReSeed newSeed ->
        printfn "Random Seed changed to %i" newSeed
        rng <- System.Random(newSeed)
        state
      | Restart ->
        let ns = blankState state
        updateView ns
        |> maybeUpdateCacheState ns        
      | NewRenderMode nrm ->
        let ns = { state with render = nrm }
        updateView ns
        |> maybeUpdateCacheState ns        
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

  let onEnterPress state = 
    let (kp', msg) = fullMessage keypress
    keypress <- kp'
    updateModel state msg

  let onkeyPress state ch = 
    let (kp', msg) = addToMessage keypress ch
    keypress <- kp'
    match msg with
    | None -> state
    | Some m2 -> updateModel state m2

