namespace FAtlas

open TriangleMeshFunctions
open TectonicTypes
open TectonicFunctions
open CoordFunctions
open TriangleMeshToRender
open ColourTypes
open CoordTypes

module TectonicViewFunctions =

  let viewClusterToBorderNodes mkVertex mkColour filterOpt (data : CompleteClusterAssignment<'A>) =
    let clusterIdToVtx i = i - 1
    let nc = (Array.length data.allClusters) - 1

    let clusterIdToColour mkColour i = 
      rangeToFullSat 0.0 (float nc) (float i) |> makeRGB |> mkColour

    let clusterToCentroid = 
      data.allClusters
      |> Array.map(fun x -> 
        x.members
        |> List.map(urlToCartesian data.meshData 1.0)
        |> centroid
        |> fun y -> (x.id, y))
      |> Map.ofArray

    let vertices = 
      data.allClusters
      |> Array.map (fun x -> 
        let cent = Map.find x.id clusterToCentroid
        mkVertex (float32 cent.x, float32 cent.y, float32 cent.z))
      |> List.ofArray
      
    let colours = 
      data.allClusters
      |> Array.map (fun x -> clusterIdToColour mkColour x.id)
      |> List.ofArray

    let addVtxState (vtxMap,vtxList,colourList,nVtx) (id, nUrl) =
      let nextId = nVtx
      let vM' = Map.add nUrl nextId vtxMap
      let newVertex = urlToCartesian data.meshData 0.7 nUrl
      let newColour = clusterIdToColour mkColour id
      let nV' = mkVertex (float32 newVertex.x, float32 newVertex.y, float32 newVertex.z)
      let vL' = vtxList @ [nV']
      let cL' = colourList @ [newColour]
      ((vM', vL', cL', nVtx+1), nextId)

    let leftBorders = List.map fst data.connections
    let rightBorders = List.map snd data.connections

    let maybeFilter = 
      match filterOpt with
      | Some(i) -> List.filter (fun (j,_) -> i = j) 
      | None -> id

    let maybeAddVertexPair (state, acc) (lc, lv) =
      let (vtxMap,_,_,_) = state
      match Map.tryFind lv vtxMap with
      | Some id -> (state, [clusterIdToVtx lc; id] @ acc)
      | None -> 
        let (ns', id) = addVtxState state (lc,lv)
        (ns', [clusterIdToVtx lc; id] @ acc)

    let (state, acc) =
      data.connections
      |> List.map fst
      |> maybeFilter
      |> List.fold maybeAddVertexPair ((Map.empty,vertices,colours, List.length vertices),[])
          
    let ((_, vertices',colours',_), acc') =
      data.connections
      |> List.map snd
      |> maybeFilter
      |> List.fold maybeAddVertexPair  (state,acc)
    (Array.ofList vertices', Array.ofList colours', Array.ofList acc')

  let viewClustersAsNetwork mkVertex mkColour (data : CompleteClusterAssignment<'A>) =
    let clusterToCentroid = 
      data.allClusters
      |> Array.map(fun x -> 
        x.members
        |> List.map(urlToCartesian data.meshData 1.0)
        |> centroid
        |> fun y -> (x.id, y))
      |> Map.ofArray

    let nc = (Array.length data.allClusters) - 1

    let vertices = 
      data.allClusters
      |> Array.map (fun x -> 
        let cent = Map.find x.id clusterToCentroid
        mkVertex (float32 cent.x, float32 cent.y, float32 cent.z))

    let clusterIdToColour mkColour i = 
      rangeToFullSat 0.0 (float nc) (float i) |> makeRGB |> mkColour
    
    
    let colours = 
      data.allClusters
      |> Array.map (fun x -> clusterIdToColour mkColour x.id)

    let vertexIndices =
      Set.toList data.connectedFaces
      |> List.collect (fun (x,y) -> [x-1; y-1])
      |> Array.ofList
    
    (vertices, colours, vertexIndices)
    
  let drawBorderWithCoordinates vertexMaker colourer (borderSection : ClusterBoundary) =
    let borderArray = Array.ofList borderSection.pts
    let mvx pt = vertexMaker (float32 pt.x, float32 pt.y, float32 pt.z)
    let points = borderArray |> Array.map(fun pt -> mvx pt.pt) 
    
    let colours = borderArray |> Array.map (fun pt -> colourer pt.radius pt.argument)
    let N = borderArray |> Array.length
    let indexer = fun (i,_) -> [ i; (i+1) % N]
    let arrayIndices = 
      borderSection.pts 
      |> List.indexed 
      |> List.collect indexer
      |> Array.ofList
    (points, colours, arrayIndices)

  let drawSingleBorderSection toCentroid vertexMaker colourer (borderSection : ClusterBoundary) =
    let borderArray = Array.ofList borderSection.pts
    let mvx pt = vertexMaker (float32 pt.x, float32 pt.y, float32 pt.z)
    let borderPts = borderSection.pts |> List.map(fun pt -> mvx pt.pt) 
    let points = 
      if toCentroid then
        [mvx borderSection.hub] @ borderPts |> Array.ofList
      else 
        borderPts |> Array.ofList

    let colours = points |> Array.mapi colourer
    let N = borderArray |> Array.length
    let indexer = 
      if toCentroid then
        fun (i,_) -> [ i+1; (i+2) % N; 0; i+1 ]
      else
        fun (i,_) -> [ i; (i+1) % N]
    let arrayIndices = 
      borderSection.pts 
      |> List.indexed 
      |> List.collect indexer
      |> Array.ofList
    (points,colours, arrayIndices)
    
    

