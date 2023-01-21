﻿namespace FAtlas

open CoordTypes
open ColourTypes
open CoordFunctions

open TriangleMeshTypes
open TriangleMeshFunctions

open TectonicTypes

open GeoMeshTypes
open GeoMeshFunctions

open TriangleMeshToRender
open TectonicViewFunctions

open ViewHelperFunctions
open AtlasStateTypes
open ViewUtilityFunctions

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
  let drawIcosaSection toSimpleCart colourer (state : AtlasState<'V,'C>) (ts : TriangleSet<'A>) renderCacheOverride (i,t) =
      
    // let colourer ij k = (0.6f,0.2f,0.5f) |> state.callbacks.makeColour 
    
    let cacheUsed = Option.defaultValue state.renderCache renderCacheOverride
    let (converters, vertexMapOpt) = getVertexConverter cacheUsed (getNp1 t)
    let rco' = 
      match vertexMapOpt with
      | Some(vm') -> { vertexConverters = vm' } |> Some
                // vertexMapOpt will be None if getVertexConverter had a cache hit
      | None -> Some cacheUsed
    let abc = asArrays state.callbacks.makeVertex (toSimpleCart i) (colourer state.callbacks.makeColour i) converters ts.frame t
    (abc, rco')

  let drawAllIcosaSections iOpt rOpt colourer state ts =
    let toSimpleCart = 
      match rOpt with 
      | Choice1Of2 r -> fun i ij (coord : 'A) -> cartFromSphereWithRadius r coord.datum
      | Choice2Of2 f -> fun i ij (coord : 'A) -> cartFromSphereWithRadius (f i ij coord) coord.datum
    ts.triangles 
    |> Array.indexed
    |> fun tsi ->
      match iOpt with 
      | Some i -> [| tsi.[i] |]
      | None -> tsi
    |> Array.mapFold (drawIcosaSection toSimpleCart colourer state ts) None

  let drawAllIcosaSectionsGeoMesh iOpt colourer state ts =
    let toSimpleCart = fun i ij (coord : 'A) -> actualCart coord.datum
    ts.triangles 
    |> Array.indexed
    |> fun tsi ->
      match iOpt with 
      | Some i -> [| tsi.[i] |]
      | None -> tsi
    |> Array.mapFold (drawIcosaSection toSimpleCart colourer state ts) None

  let solidViewIcosaSection rOpt iOpt colourer state ts =
    let (elts, newCache) = 
      drawAllIcosaSections iOpt rOpt colourer state ts
    state.callbacks.onUpdateCallback [concat3 "Triangles" elts]
    newCache 

  let solidViewGeoMesh iOpt colourer state ts =
    let (elts, newCache) = 
      drawAllIcosaSectionsGeoMesh iOpt colourer state ts
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
      drawAllIcosaSections None (Choice1Of2 0.6) wireColourer state ts
        
    
    state.callbacks.onUpdateCallback [concat3 "Triangles" solid]
    newCache 

  let solidAndWireViewIcosaSection wireColourer state (ccs : CompleteClusterAssignment<'A>) =
    let ts = ccs.meshData

    let (w1,w2,w3) = viewClusterToBorderNodes state.callbacks.makeVertex state.callbacks.makeColour None ccs
    let lineSection = (w1,w2,w3,"Lines")
    let (solid, newCache) =
      drawAllIcosaSections None (Choice1Of2 0.6) (tectonicColours2 ccs) state ts
    
    let allElts =
      [concat3 "Triangles" solid; lineSection]

    state.callbacks.onUpdateCallback allElts
    newCache 

  
  let extractTriangleSet s = 
    match s with
    | IcosaDivision t -> t
    | ClusterAssignment (cas, vc) -> cas.meshData
    | ClusterFinished cf -> cf.meshData
    | TectonicAssigned tec -> tec.cca.meshData
    | GeoDivision gds -> gds.tectData.cca.meshData
    | _ -> failwith <| sprintf "No triangles set for %A" s

  let extractFineTriangleSet s = 
    match s with
    | GeoDivision gds -> gds.triangleSet
    | _ -> failwith <| sprintf "No fine triangles set for %A" s
    
  let extractClusterData s =
    match s with
    | ClusterAssignment (cas,_) -> renderCAS cas
    | ClusterFinished cf -> renderCCS cf
    | TectonicAssigned tec -> tec.cca |> renderCCS
    | GeoDivision gds -> gds.tectData.cca |> renderCCS
    | _ -> failwith <| sprintf "No cluster data for %A" s
        
  let extractCompleteClusterData s =
    match s with
    | ClusterFinished cf -> cf
    | TectonicAssigned td -> td.cca
    | GeoDivision gds -> gds.tectData.cca
    | _ -> failwith <| sprintf "No cluster data for %A" s

  let extractTectonicData s =
    match s with
    | TectonicAssigned td -> td
    | GeoDivision gds -> gds.tectData
    | _ -> failwith <| sprintf "No tectonic data for %A" s
    
  let extractGeoMesh s = 
    match s with
    | GeoDivision gds -> gds.triangleSet
    | _ -> failwith <| sprintf "No GeoMesh set for %A" s

  let getColourer cs state =
    match cs with 
    | GrayScale ->        uniformHue 20.0
    | TectonicColours None ->  tectonicColours <| extractClusterData state.model
    | TectonicColours (Some targetIdx) ->  
      let cd = extractClusterData state.model
      let targetId = targetIdx + 1
      tectonicColoursFiltered targetId <| cd
    | TectonicLocalCoordColours None ->  tectonicRThColours (extractTriangleSet state.model) None <| extractCompleteClusterData state.model
    | TectonicLocalCoordColours (Some targetIdx) ->  
      let targetId = targetIdx + 1
      tectonicRThColours (extractTriangleSet state.model) (Some targetId) <| extractCompleteClusterData state.model
    | TectonicStressColours iOpt -> 
      tectonicStressColours (extractTriangleSet state.model) (iOpt |> Option.map(fun i -> i+1)) <| extractTectonicData state.model
    | TectonicHeightBiasColours (iOpt, hbType, _) ->
      match hbType with
      | HB_Flat -> tectonicFlatHeight (extractTriangleSet state.model) (iOpt |> Option.map(fun i -> i+1)) <| extractTectonicData state.model
      | HB_None -> tectonicColours <| extractClusterData state.model
      | _ -> failwith "nyi"
    | TectonicHeightBias (iOpt) ->
      tectonicHeightColours 1.0 (extractTriangleSet state.model) (iOpt |> Option.map(fun i -> i+1)) <| extractTectonicData state.model
    
    
  let updateIcosaView iOpt cs state =
    let colours = getColourer cs state
    let rOpt = 
      match cs with
      | TectonicHeightBiasColours (jOpt, _, x) -> 
          let fn = 
            match x with
            | HB_Flat -> getFlatHeightBias
            | HB_Linear -> getLinearHeightBias
            | HB_Stressed -> getStressedHeightBias
            | HB_None -> fun x y -> (0.0, 0.0/1.0)
          Choice2Of2 <| (tectonicRadiusBiasFlatHeight 1.0 fn (extractTriangleSet state.model) (jOpt |> Option.map(fun i -> i+1)) <| extractTectonicData state.model)
      | TectonicHeightBias _ ->
        Choice2Of2 <| (tectonicRadiusBiasFlatHeight 1.0 getStressedHeightBias (extractTriangleSet state.model) (iOpt |> Option.map(fun i -> i+1)) <| extractTectonicData state.model)        
      | _ -> Choice1Of2  1.0

    solidViewIcosaSection rOpt iOpt colours state (extractTriangleSet state.model)

    
  let updateGeoMeshView iOpt cs state =
    let colours = getColourer cs state
    let colours' = maybeRescaleFunction (extractTriangleSet state.model) (extractFineTriangleSet state.model) colours
    solidViewGeoMesh iOpt colours' state (extractGeoMesh state.model)
        
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
      drawAllIcosaSections None (Choice1Of2 0.8) grayscale state (extractTriangleSet state.model)
    let borderSections =
      match bva with
      | (JustBorder, iOpt) ->
        drawSimpleBorder state.callbacks.makeVertex state.callbacks.makeColour iOpt (extractCompleteClusterData state.model)
      | (_, iOpt) -> 
        drawParamBorder state.callbacks.makeVertex state.callbacks.makeColour iOpt (extractCompleteClusterData state.model)

    state.callbacks.onUpdateCallback [concat3 "Triangles" elts; concat3 "Lines" borderSections]
    newCache