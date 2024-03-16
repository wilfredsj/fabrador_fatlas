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
open FAtlas.TriangleTypes

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

let myInit geoMesh seed =
  let ms = initState emptyCallback
  let initScript = 
    [ReSeed seed; Divide 4; ClusterInit None; ClusterIterate 5000] @ if(geoMesh) then [AssignTectonics; InitGeoMesh false; Divide 2] else []
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
  let problemCase = myInit false 1138
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

// AETHER-89
[<Test>]
let TestRadiusFunction () =
  let problemCase = myInit true 10101
  let clusterData = extractCompleteClusterData problemCase.model

  let clusterToCheck = clusterData.allClusters |> Array.find(fun c -> c.id = 6)

  let problemPoint = cart 0.5633320074 0.2323790856 0.6209747922

  let radiusFunction = getLocalCoordinates clusterToCheck.orderedBorder

  let actualBorderLength =
    clusterToCheck.orderedBorder.pts
    |> pairwiseWithCyclic None
    |> List.fold (fun acc (p1, p2) -> acc + modulus (polar p1.radius p1.argument - polar p2.radius p2.argument)) 0.0

  let normalizedBorderLength =
    clusterToCheck.orderedBorder.normalizedBoundary
    |> Array.fold (fun acc (th,(r,nbs)) -> acc + modulus (polar nbs.y_lower nbs.x_upper - polar nbs.y_upper nbs.x_lower)) 0.0

  let print = false
  if print then
    printfn "ai,aj,at,mx,my,mz,,,bi,bj,bt, th,r"
    clusterToCheck.orderedBorder.pts
    |> List.iter(fun p -> printfn "%i,%i,%i,%-.5f,%-.5f,%-.5f,,,%i,%i,%i,,%-.3f,%-.3f" p.inUrl.i p.inUrl.j p.inUrl.t p.pt.x p.pt.y p.pt.z p.outUrl.i p.outUrl.j p.outUrl.t p.argument p.radius)
  
    printfn "th,r,,th_l,r_l,,th_u,r_u,,dxdy"
    clusterToCheck.orderedBorder.normalizedBoundary
    |> Array.iter(fun (th,(r, ob)) -> printfn "%-.3f,%-.3f,,%-.3f,%-.3f,,%-.3f,%-.3f,,%-.3f" th r ob.x_lower ob.y_lower ob.y_lower ob.y_upper ob.dy_dx)

    
  Assert.AreEqual(actualBorderLength, normalizedBorderLength, 0.1 * actualBorderLength)


