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