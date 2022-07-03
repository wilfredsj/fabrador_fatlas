namespace FAtlas

open CoordTypes
open ColourTypes
open TriangleMeshTypes
open TectonicTypes
open CoordFunctions
open TriangleMeshFunctions

open TriangleMeshToRender
open TectonicViewFunctions

open ViewHelperFunctions
open AtlasStateTypes

module AtlasViewFunctions =

  let getNp1 t =
    t.points.Length
    
  let getNp1TS ts = 
    getNp1 ts.triangles.[0]

  let getVertexConverter cache np1 =
    match Map.tryFind np1 cache.vertexConverters with
    | Some(vc) -> (vc, None)
    | None ->
      let newElt = vertexConverters np1
      let vcs' = Map.add np1 newElt cache.vertexConverters
      (newElt, Some(vcs'))

  // Typical use case is in a fold
  //    in which case renderCacheOverride is None on the first TriangleSet
  //                                 and non-None on the subsequent ones
  let drawIcosaSection r colourer (state : AtlasState<'V,'C>) (ts : TriangleSet<'A>) renderCacheOverride (i,t) =
    let toSimpleCart ij (coord : KeyedPoint<Coordinate>) = cartFromSphereWithRadius r coord.datum
    
    // let colourer ij k = (0.6f,0.2f,0.5f) |> state.callbacks.makeColour 
    
    let cacheUsed = Option.defaultValue state.renderCache renderCacheOverride
    let (converters, vertexMapOpt) = getVertexConverter cacheUsed (getNp1 t)
    let rco' = 
      match vertexMapOpt with
      | Some(vm') -> { vertexConverters = vm' } |> Some
                // vertexMapOpt will be None if getVertexConverter had a cache hit
      | None -> Some cacheUsed
    let abc = asArrays state.callbacks.makeVertex toSimpleCart (colourer state.callbacks.makeColour i) converters ts.frame t
    (abc, rco')

  let drawAllIcosaSections r colourer state ts =
    ts.triangles 
    |> Array.indexed
    |> Array.mapFold (drawIcosaSection r colourer state ts) None

  let solidViewIcosaSection colourer state ts =
    let (elts, newCache) = 
      drawAllIcosaSections 1.0 colourer state ts
    state.callbacks.onUpdateCallback [concat3 "Triangles" elts]
    newCache 

  let solidViewMercator state = 
    
    let pointMaker (lat,long) = { latitude = lat; longitude = long } |> cartFromSphereWithRadius 1.0
    let pm' = pointMaker >> cartToExternal state.callbacks.makeVertex

    let colourMaker (lat,long) = rangeToFullSat 0.0 6.0 long |> makeRGB|> state.callbacks.makeColour
    let viewData = MercatorViewFunctions.createSolid pm' colourMaker 21 21
    state.callbacks.onUpdateCallback [viewData]
    ()
    
  let solidViewIcosaSectionNoWires wireColourer state (ccs : CompleteClusterAssignment<'A>) =
    let ts = ccs.meshData
    let (solid, newCache) =
      drawAllIcosaSections 0.6 wireColourer state ts
        
    
    state.callbacks.onUpdateCallback [concat3 "Triangles" solid]
    newCache 

  let solidAndWireViewIcosaSection wireColourer state (ccs : CompleteClusterAssignment<'A>) =
    let ts = ccs.meshData

    let (w1,w2,w3) = viewClusterToBorderNodes state.callbacks.makeVertex state.callbacks.makeColour None ccs
    let lineSection = (w1,w2,w3,"Lines")
    let (solid, newCache) =
      drawAllIcosaSections 0.6 (tectonicColours2 ccs) state ts
    
    let allElts =
      [concat3 "Triangles" solid; lineSection]

    state.callbacks.onUpdateCallback allElts
    newCache 

  
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
    
  let updateIcosaView cs state =
    let colours = 
      match cs with 
      | GrayScale ->        uniformHue 20.0
      | TectonicColours None ->  tectonicColours <| extractClusterData state.model
      | TectonicColours (Some targetIdx) ->  
        let cd = extractClusterData state.model
        let targetId = targetIdx + 1
        tectonicColoursFiltered targetId <| cd
    solidViewIcosaSection colours state (extractTriangleSet state.model)
        
  let updateClusterView cs state =
    let colours = 
      match cs.colours with
      | TectonicColours jOpt ->   (tectonicColours2 <| extractCompleteClusterData state.model)
      | _ ->                 failwith "Bad render combination"
    if cs.wireframeConnections then
      solidAndWireViewIcosaSection colours state (extractCompleteClusterData state.model)
    else
      solidViewIcosaSectionNoWires colours state (extractCompleteClusterData state.model)

  
  let drawParamBorder mkVertex mkColour jOpt (clusterState : CompleteClusterAssignment<'A>) =
    let nc = clusterState.allClusters |> Array.length
    let colour r th = 
      { hue = th |> float32; sat = r |> float32; value = 1.0f } |> makeRGB |> mkColour
    let renderData =
      match jOpt with
      | Some j ->
        let j' = j % nc
        [| drawBorderWithCoordinates mkVertex colour clusterState.allClusters.[j'].orderedBorder |]
      | None ->
        clusterState.allClusters
        |> Array.indexed
        |> Array.fold(
          fun acc (i,cluster) ->
            let newDraw = drawBorderWithCoordinates mkVertex colour cluster.orderedBorder
            newDraw :: acc) []
        |> Array.ofList
    renderData

  let drawSimpleBorder mkVertex mkColour jOpt (clusterState : CompleteClusterAssignment<'A>) =
    let nc = clusterState.allClusters |> Array.length
    let clusterColour ci = rangeToFullSat 0.0 (float nc) (float ci) |> makeRGB|> mkColour
    let renderData =
      match jOpt with
      | Some j ->
        let j' = j % nc
        //let colour = clusterColour j'
        [| drawSingleBorderSection true mkVertex (fun segNum _ -> clusterColour (segNum % nc)) clusterState.allClusters.[j'].orderedBorder |]
      | None ->
        clusterState.allClusters
        |> Array.indexed
        |> Array.fold(
          fun acc (i,cluster) ->
            let colour = clusterColour i
            let newDraw = drawSingleBorderSection false mkVertex (fun _ _ -> colour) cluster.orderedBorder
            newDraw :: acc) []
        |> Array.ofList
    renderData

  let updateBorderView bva state =
    let (elts, newCache) = 
      drawAllIcosaSections 0.8 grayscale state (extractTriangleSet state.model)
    let borderSections =
      match bva with
      | (JustBorder, iOpt) ->
        drawSimpleBorder state.callbacks.makeVertex state.callbacks.makeColour iOpt (extractCompleteClusterData state.model)
      | (_, iOpt) -> 
        drawParamBorder state.callbacks.makeVertex state.callbacks.makeColour iOpt (extractCompleteClusterData state.model)

    state.callbacks.onUpdateCallback [concat3 "Triangles" elts; concat3 "Lines" borderSections]
    newCache