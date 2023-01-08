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
    | ClusterAssignment _ ->     IcosaView (TectonicColours None)
    | ClusterFinished _ ->       ClusterView { colours = (TectonicColours None); wireframeConnections = true }
    | TectonicAssigned _ ->      IcosaView GrayScale
  
  let isSupportedRenderState state =
    match state.render with
    | MercatorView ->      true
    | IcosaView GrayScale -> true
    | IcosaView (TectonicColours _) ->
      match state.model with
      | ClusterAssignment _
      | ClusterFinished _ 
      | TectonicAssigned _ -> true
      | _ -> false
    | IcosaViewFiltered ((TectonicColours _), _) ->
      match state.model with
      | ClusterAssignment _
      | ClusterFinished _ 
      | TectonicAssigned _ -> true
      | _ -> false
    | IcosaView (TectonicLocalCoordColours _)
    | IcosaViewFiltered ((TectonicLocalCoordColours _), _) ->
      match state.model with
      | ClusterFinished _ -> true
      | TectonicAssigned _ -> true
      | _ -> false
    | IcosaView (TectonicStressColours _)
    | IcosaView (TectonicHeightBiasColours _)
    | IcosaViewFiltered ((TectonicStressColours _), _)
    | IcosaViewFiltered ((TectonicHeightBiasColours _), _) ->
      match state.model with
      | TectonicAssigned _ -> true
      | _ -> false

    | ClusterView _ ->
      match state.model with
      | ClusterFinished _ -> true
      | _ -> false
    | BorderView _ ->
      match state.model with
      | ClusterFinished _ -> true
      | _ -> false
    | _ -> false

  let getRenderMode state =
    if isSupportedRenderState state then
      state.render
    else
      defaultRenderMode state


  let updateView state = 
    let renderModeUsed = getRenderMode state
    match renderModeUsed with
    | IcosaView cs ->       updateIcosaView None cs state 
    | IcosaViewFiltered (cs,i) ->       updateIcosaView (Some i) cs state 
    | ClusterView cs ->     updateClusterView cs state
    | MercatorView ->
        solidViewMercator state
        None
    | BorderView (bvm, iOpt) -> updateBorderView (bvm,iOpt) state
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

  let assignTectonicState state (cca : CompleteClusterAssignment<(char*int) list>) =
    let tec = makeTectonics rng cca
    { state with model = TectonicAssigned tec}

  let blankState state =
    let data = createIcosahedron()
    let newModel = IcosaDivision data
    { state with model = newModel }

  let evaluateNewRenderMode (newRenderMode : RenderMode) (oldRenderMode : RenderMode) =
    match newRenderMode with
    | BorderView (viewType, (Some x)) ->
      if x < 0 then
        match oldRenderMode with
        | BorderView (oldViewType, yOpt) ->
          match yOpt with
          | Some y -> 
              if x = -2 then 
                BorderView(oldViewType, Some (y + 1))
              else
                BorderView(oldViewType, Some (y - 1))
          | None ->
            BorderView( oldViewType, Some 0)
        | _ ->
          BorderView(viewType, Some 0)
      else
        newRenderMode
    | BorderView (newViewType, None) ->
        match oldRenderMode with
        | BorderView (_, oldFilter) -> BorderView(newViewType, oldFilter)
        | _                         -> newRenderMode
    | _ -> newRenderMode

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
      | UIInstruction uix ->
        match state.callbacks.uiCallbackOpt with
        | Some uiCallbacks ->
          match uix with
          | ForceEuclidian ->
            uiCallbacks.forceEuclidian()
          | ForceMercator ->
            uiCallbacks.forceMercator()
          | ForceRotate rot ->
            uiCallbacks.forceRotation rot
        | None -> ()
        state

      | NewRenderMode nrmIn ->
        let nrm = evaluateNewRenderMode nrmIn state.render
        printfn "New Render Mode: %A" (sprintRenderMode nrm)
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
      | AssignTectonics ->
        match state.model with
        | ClusterFinished cca ->
          let ns = assignTectonicState state cca
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

