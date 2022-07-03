module FAtlas21Core.TectonicTests

open NUnit.Framework

open FAtlas.Interface
open FAtlas.AtlasStateTypes
open FAtlas.AtlasIO
open FAtlas.TectonicFunctions
open FAtlas.TectonicTypes
open FAtlas.TriangleMeshFunctions
open FAtlas.CoordFunctions
open FAtlas.AtlasViewFunctions
open FAtlas.TriangleMeshTypes

let createSetupIcosahedron n =    
  let data = createIcosahedron()
  let divided = 
    [1 .. n] 
    |> List.fold (fun t i -> divideIcosahedron t) data
  divided


let emptyCallback = { makeVertex = (fun x -> 1); 
                    makeColour = (fun x -> 1); 
                    onUpdateCallback = (fun x -> ()); 
                    uiCallbackOpt = None }

let myInit seed =
  let ms = initState emptyCallback
  let initScript = [ReSeed seed; Divide 4; ClusterInit None; ClusterIterate 5000] 
  let model' = initScript |> List.fold updateModel ms
  model'

let rec sprintWithin (accIn : string list) ts lookup clusterId n url =
  if n <= 0 then
    accIn
  else
    getVertexNeighboursFromUrl ts url
    |> List.filter(fun nurl -> Map.find nurl lookup = clusterId)
    |> List.fold(fun acc nurl ->
      let acc' = (sprintf "||%i|| %s -> %s" n (vtxStr url) (vtxStr nurl)) :: acc
      sprintWithin acc' ts lookup clusterId (n-1) nurl) accIn


let debugCreateBorder (clusterdata : CompleteClusterAssignment<'A>) c =
  let ts = clusterdata.meshData
  let centroid = makeCentroid ts c.members
  let connectionMap =
    clusterdata.connections
    |> List.collect(fun ((c1, v1), (c2, v2)) ->
        [(c1, v1); (c2, v2)])
    |> List.distinctBy(fst)
    |> Map.ofList
  let sampleBorderVertex = Map.find c.id connectionMap
  let normalized = sampleBorderVertex |> normalizeElement ts

  let lookup = clusterdata.clusterAssignments
  
  let neighbours = getVertexNeighboursFromUrl ts sampleBorderVertex
  let res = sprintWithin [] ts lookup c.id 3 sampleBorderVertex

  // Adjacency appears to be:

  // 
  //              
  //                                      /--- (1, 13)[7]
  //   HUB (1, 10)[7]     /--- (1, 12)[7]     
  //          \\         /          |     \
  //            (1, 11)[7]          |       ---(0, 13)[7]
  //                     \          |     / 
  //                      \--- (0, 12)[7]     

  //let areTheyOnTheBorder = neighbours |> List.map(fun nurl -> (nurl, Map.find nurl lookup, Map.find nurl lookup = c.id ))
  //let normalizedOnes = neighbours |> List.map(normalizeElement ts)
  //let normalizedOnBorder = normalizedOnes |> List.map(fun nurl -> (nurl, (Map.find nurl lookup) = c.id ))

  //let next = { t = 7; i = 1; j = 11 }
  //let neighbours1 = getVertexNeighboursFromUrl ts next
  //let areTheyOnTheBorder1 = neighbours1 |> List.map(fun nurl -> (nurl, Map.find nurl lookup, Map.find nurl lookup = c.id ))



  createClusterBoundary ts lookup centroid c.id sampleBorderVertex

// AETHER-31
[<Test>]
let TestBorder() =
  let problemCase = myInit 1138
  let clusterData = extractCompleteClusterData problemCase.model
  let allRegisteredAsEdges = clusterData.connections |> List.collect(fun pair -> [fst pair; snd pair])
  // Just to check if this is base-0 or base-1
  let minClusterId = allRegisteredAsEdges |> List.minBy(fst) |> fst
  Assert.AreEqual(minClusterId, 1)

  let getLocalBorderLength base1 =
    clusterData.allClusters.[base1-1].orderedBorder.pts |> List.length

  let getGlobalBorderLength base1 =
    allRegisteredAsEdges |> List.filter(fun (i,url) -> i = base1) |> List.length

  // I suppose every element from the global list should appear on the local list, too
  // However there probably are cases where the border goes through a vertex twice (e.g. 8-shaped region)
  // Hence check l >= g rather than l = g
  let (good, bad) = 
    clusterData.allClusters
    |> Array.mapi(fun i cluster ->
      let x = getLocalBorderLength cluster.id
      let y = getGlobalBorderLength cluster.id
      (i, cluster.id, x, y))
    |> Array.partition(fun (i1, i2, l, g) -> l >= g)

  let test = debugCreateBorder clusterData clusterData.allClusters.[9]

  Assert.AreEqual(Array.length bad, 0)
