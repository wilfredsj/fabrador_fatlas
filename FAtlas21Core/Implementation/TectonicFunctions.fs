namespace FAtlas

open TriangleMeshFunctions
open TectonicTypes
open TriangleMeshTypes
open CoordFunctions
open CoordTypes

module TectonicFunctions =
  let maker keys cart =
    { key = keys; datum = coordFromCart cart}
    
    
  let printKeyWithPadding n point =
    let proto =
      point.key
      |> List.collect(fun (c,i) -> List.replicate i c)
    let nPad = max (n - List.length proto) 0
    let padding = List.replicate nPad ' '
    ['|'] @ proto @ padding |> System.String.Concat
    
      
  let sprintTriangleKeys t =
    let N = t.points |> Array.length |> fun x -> x - 1
    //let n = 1 + System.Math.ILogB (float N)
    [0 .. N]
    |> List.map(fun row ->
      [0 .. (N - row)]
      |> List.map(fun col -> printKeyWithPadding N t.points.[row].[col])
      |> System.String.Concat)
  let createIcosahedron () =
    TriangleMeshFunctions.createIcosahedronSet maker

  let interpolate (a : KeyedPoint<Coordinate>) (b : KeyedPoint<Coordinate>) newKey =
    let midPoint = (cartFromSphere a.datum + (cartFromSphere b.datum)) * 0.5
    maker newKey midPoint

  let divideIcosahedron ts =
    let initCache = Map.empty<(char*int) list, KeyedPoint<Coordinate>>
    let getKey t = t.key
    let res = singleDivideTriangleSet initCache (fun ts -> makeEmptyTriangle ts.keys) getKey interpolate ts
    sprintTriangleKeys res.triangles.[0]
    |> List.iter System.Console.WriteLine
    res

  let makeCluster ts id hub = 
    let border = getVertexNeighboursFromUrl ts hub |> Set.ofList
    { id = id ; members = [hub]; borderValencyOne = border; borderValencyTwo = Set.empty; borderValencyThree = Set.empty }

  let addToBorders (b1,b2,b3) url =
    if Set.contains url b3 then
      (b1,b2,b3)
    elif Set.contains url b2 then
      (b1, Set.remove url b2, Set.add url b3)
    elif Set.contains url b1 then
      (Set.remove url b1, Set.add url b2, b3)
    else
      (Set.add url b1, b2, b3)

  let addVertexToCluster ts cluster membership newVertex = 
    let border1' = 
      cluster.borderValencyOne
      |> Set.remove newVertex

    let border2' = 
      cluster.borderValencyTwo
      |> Set.remove newVertex

    let border3' = 
      cluster.borderValencyThree
      |> Set.remove newVertex
    
    let (existing, unmapped) = 
      getVertexNeighboursFromUrl ts newVertex
      |> List.partition(fun url -> Map.containsKey url membership)

    let newConnections =
      existing
      |> List.collect(fun url -> 
        let x = Map.find url membership
        if x <> cluster.id then
          [((cluster.id, newVertex), (x, url))]
        else
          [])
    let (b1'', b2'', b3'') = 
      unmapped
      |> List.fold addToBorders (border1', border2', border3')
      
    (newConnections, { cluster with members = newVertex :: cluster.members; borderValencyOne = b1''; borderValencyTwo = b2''; borderValencyThree = b3'' })

  let createOneNewCluster rng oneDtoTwoD ts existingMembers id = 
    let candidate = 
      [0 .. 10]
      |> List.tryPick(fun _ ->
        let url = uniformCoordinate rng oneDtoTwoD ts 
        if Map.containsKey url existingMembers then
          None
        else
          Some url)
    match candidate with
    | None -> failwith "Couldn't find unassigned element"
    | Some(c) ->
      let cluster = makeCluster ts id c
      let newMembers = Map.add c id existingMembers
      (cluster, newMembers)

  let initializePartitions rng oneDtoTwoD ts numPartitions =
    let (clusters, members) = 
      [1 .. numPartitions]
      |> List.mapFold (createOneNewCluster rng oneDtoTwoD ts) Map.empty
    let keyMaker = urlToKey ts
    let m2 = members |> Map.toSeq |> Seq.map(fun (u,v) -> (keyMaker u, v)) |> Map.ofSeq
    let clusterArr = Array.ofList clusters
    {
      meshData = ts;
      clusterAssignments = members; 
      lookupFromKey = m2;
      allClusters = clusterArr;
      unfinishedClusters = Array.init (Array.length clusterArr) id;
      connectionsSoFar = List.empty;
      connectedFaces = Set.empty
    }

  let getSetToPickFrom (rng : System.Random) cluster =
    if Set.isEmpty cluster.borderValencyThree |> not then
      (3, cluster.borderValencyThree)
    elif Set.isEmpty cluster.borderValencyTwo |> not then
      if Set.isEmpty cluster.borderValencyOne then
        (2, cluster.borderValencyTwo)
      else
        let b2 = float <| Set.count cluster.borderValencyTwo
        let relativeTwoWeight = 3.0
        let a = (relativeTwoWeight * b2) / (relativeTwoWeight * b2 + (float <| Set.count cluster.borderValencyOne))
        if rng.NextDouble() < a then
          (2, cluster.borderValencyTwo)
        else
          (1, cluster.borderValencyOne)
     else
      (1, cluster.borderValencyOne)

  let expandCluster (rng : System.Random) ts existingMapping cluster =
    if finishedCluster cluster then
      (None, cluster, [])
    else
      let (indic, set) = getSetToPickFrom rng cluster
      let arr = Set.toArray set
      let j = rng.Next(0, Array.length arr)
      let candidate = arr.[j]
      match Map.tryFind candidate existingMapping with
      | Some(_) ->
        let borderUpdate = 
          match indic with
          | 1 -> { cluster with borderValencyOne = Set.remove candidate cluster.borderValencyOne }
          | 2 -> { cluster with borderValencyTwo = Set.remove candidate cluster.borderValencyTwo }
          | 3 -> { cluster with borderValencyThree = Set.remove candidate cluster.borderValencyThree }
          | _ -> failwith <| sprintf "Logic Error %i" indic
        (None, borderUpdate, [])
      | None ->
        let (newConnections, cluster') = addVertexToCluster ts cluster existingMapping arr.[j]
        (Some (arr.[j], cluster.id), cluster', newConnections)
    
  let expandOneCluster (rng : System.Random) ts clusterData =
    let nClusters = clusterData.unfinishedClusters |> Array.length
    if nClusters > 0 then 
      let u_i = rng.Next(0,nClusters)
      let i = clusterData.unfinishedClusters.[u_i]
      let (newOpt, cluster', newConnections) = 
        expandCluster rng ts clusterData.clusterAssignments clusterData.allClusters.[i]
    
      let clusters' = 
        match newOpt with
        | Some (nu ,nv) ->       
          let nk = urlToKey ts nu
          let newFaceConnections = List.fold(fun s ((e1,f1),(e2,f2)) -> Set.add (e1,e2) (Set.add (e2,e1) s)) clusterData.connectedFaces newConnections
          let unfinished' = 
            if finishedCluster cluster' then
              Array.filter (fun j -> j <> i) clusterData.unfinishedClusters
            else
              clusterData.unfinishedClusters
          {       
          clusterData with clusterAssignments = Map.add nu nv clusterData.clusterAssignments ;
                           lookupFromKey = Map.add nk nv clusterData.lookupFromKey ;
                           connectionsSoFar = newConnections @ clusterData.connectionsSoFar ;
                           connectedFaces = newFaceConnections ;
                           unfinishedClusters = unfinished'
          }
        | None -> 
          if finishedCluster cluster' then
            { clusterData with unfinishedClusters = Array.filter (fun j -> j <> i) clusterData.unfinishedClusters }
          else
            clusterData
      clusters'.allClusters.[i] <- cluster'
      clusters'
    else
      clusterData

  let defaultNumClusters ts = 
    let Np1 = Array.length ts.triangles.[0].points
    let totalApprox = 10 * (Np1 * (Np1 + 1))
    let heuristic =
      totalApprox
      |> float32
      |> sqrt
      |> fun x -> if x > 25.0f then x / 5.0f + 20.f else x
    int heuristic

  let urlToCartesian (ts : TriangleSet<KeyedPoint<Coordinate>>) r url = 
    let t = ts.triangles.[url.t]
    let p = t.points.[url.i].[url.j].datum
    cartFromSphereWithRadius r p

  let rec accumulateSomethingUntil' acc ontrue onfalse predicate list =
    match list with
    | [] -> acc
    | h :: tail ->
      if predicate h then
        ontrue acc h
      else
        accumulateSomethingUntil' ((onfalse h) :: acc) ontrue onfalse predicate tail
    
  let accumulateSomethingUntil ontrue onfalse predicate list =
    accumulateSomethingUntil' [] ontrue onfalse predicate list
      
  let accumulateSomethingButSkipHeadIfTrue acc ontrue onfalse predicate list =
    match list with
    | [] -> []
    | h :: tail ->
      if predicate h then
        accumulateSomethingUntil' acc ontrue onfalse predicate tail
      else
        accumulateSomethingUntil' acc ontrue onfalse predicate list

  let createClusterBoundary ts (lookup : Map<VertexUrl,int>) centroid thisClusterId firstUrl' =
    let firstUrl = normalizeElement ts firstUrl'

    let urlToCart_local = urlToCartesian ts 1.0

    let getSortedNeighbours samplePointOpt url = 
      let neighboursAndIsInternal = 
        getVertexNeighboursFromUrl ts url
        |> List.map(fun nurl -> (nurl, (Map.find nurl lookup) = thisClusterId ))
      let hub = urlToCart_local  url
      let samplePoint = 
        Option.defaultWith (fun () -> neighboursAndIsInternal |> List.filter(snd) |> List.head |> fst) samplePointOpt
        |> urlToCart_local 
      getBearings hub samplePoint (fst >> urlToCart_local) neighboursAndIsInternal
      |> List.sortBy snd

    let initialNeighbours = 
      getSortedNeighbours None firstUrl

    let duplicatedNeighbours = 
      initialNeighbours @ [List.head initialNeighbours]

    let referencePoint = 
      duplicatedNeighbours
      |> List.pairwise
      |> List.tryPick(
        fun (((u1, sc1), b1), ((u2, sc2), b2)) -> 
          if ((sc1 = true) && (sc2 = false)) then
            Some u1
          else 
            None)
      |> function
         | Some x -> x
         | None -> failwith "Logic error, should be at least 1 external point"

    let referenceCart = urlToCart_local referencePoint

    let bearinger = makeBearinger centroid referenceCart
    

    let makeBoundaryPoint inPoint inURL outUrl = 
      let outPoint = urlToCart_local outUrl
      let midPoint = scale (inPoint + outPoint) 0.5
      let radius = modulus (midPoint - centroid)
      { pt = midPoint; inUrl = inURL; outUrl = outUrl; radius = radius; argument = bearinger midPoint }
      

    let rec foldClusterBoundary' initialPair previousPoint listSoFar vertexUrl =
      printfn "Hub = %s" <| vtxStr vertexUrl
      printfn "NormHub = %s" <| (normalizeElement ts vertexUrl |> vtxStr)
      printfn "Previous = %s" <| vtxStr previousPoint
      printfn "NormPrev = %s" <| (normalizeElement ts previousPoint |> vtxStr)
      printfn "Check hub is internal %b" <| ((Map.find vertexUrl lookup) = thisClusterId)
      printfn "Boundary so far:"
      
      listSoFar
      |> List.iter(printfn "%s" << bpStr)
      let matchedStartingPoint =
        Some (previousPoint, vertexUrl) = initialPair
      if matchedStartingPoint then
        listSoFar
      else
        let i' = 
          match initialPair with
          | None -> Some (previousPoint, vertexUrl)
          | x -> x
        let neighboursAndIsInternal = 
          getVertexNeighboursFromUrl ts vertexUrl
          |> List.map(fun nurl -> (nurl, (Map.find nurl lookup) = thisClusterId ))
        let hub = urlToCartesian ts 1.0 vertexUrl
        let samplePoint = urlToCartesian ts 1.0 previousPoint
        let withBearings = 
          getBearings hub samplePoint (fst >> urlToCartesian ts 1.0) neighboursAndIsInternal
          |> List.sortBy snd
        let [(_, consistencyBearing)] = getBearings hub samplePoint (urlToCartesian ts 1.0) [previousPoint]
        printfn "Check %f" consistencyBearing
        let isInside = fun ((_, a), _) -> a = true
        withBearings
        |> List.iter(fun ((vtx, inflag), b) ->
          printfn "%f (%b): %s " b inflag <| vtxStr vtx)
        let outsideAction ((outUrl, _), _) =
          makeBoundaryPoint hub vertexUrl outUrl
        let insideAction acc ((inUrl, _), _) =
          foldClusterBoundary' i' vertexUrl acc inUrl
        accumulateSomethingButSkipHeadIfTrue listSoFar insideAction outsideAction isInside withBearings

    let points = foldClusterBoundary' None referencePoint [] firstUrl
    { pts = points; hub = centroid; ref = referenceCart }

  let finaliseCluster boundaries (icd : IncompleteClusterDatum) = 
    { 
      id = icd.id;
      members = icd.members
      orderedBorder = Map.find icd.id boundaries
    }
  
  let makeCentroid ts (vtxs : VertexUrl list) =
    let urlToCart_local = urlToCartesian ts 1.0
    vtxs 
    |> List.map(urlToCart_local)
    |> centroid
    
  let finalize (u : ClusterAssigmentState<'A>) = 
    let ts = u.meshData
    let centroids = 
      u.allClusters
      |> Array.map(fun c ->
        (c.id, makeCentroid ts c.members))
      |> Map.ofArray
    let boundaries =
      u.connectionsSoFar
      |> List.collect(fun ((c1, v1), (c2, v2)) ->
          [(c1, v1); (c2, v2)])
      |> List.distinctBy(fst)
      |> List.map(fun (clusterId, sampleBorderVertex) ->
        let centroid = Map.find clusterId centroids
        (clusterId, createClusterBoundary ts u.clusterAssignments centroid clusterId sampleBorderVertex))
      |> Map.ofList
    { 
      connections = u.connectionsSoFar ;
      meshData = u.meshData;
      clusterAssignments = u.clusterAssignments ;
      lookupFromKey = u.lookupFromKey ;
      connectedFaces = u.connectedFaces ;
      allClusters = u.allClusters |> Array.map(finaliseCluster boundaries)
    }