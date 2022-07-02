namespace FAtlas


open CoordTypes
open ColourTypes
open TriangleMeshTypes
open TectonicTypes
open CoordFunctions
open TriangleMeshFunctions

open TriangleMeshToRender
open TectonicViewFunctions

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

  let tectonicColours (clusterState : ClusterDataForRendering<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.numClusters |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.membership
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