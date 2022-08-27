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
    singleDivideTriangleSet initCache (fun ts -> makeEmptyTriangle ts.keys) getKey interpolate ts

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

  let pairwiseWithCyclic_Reversed l =
    let rec pairwiseWithCyclic' first previous acc l' =
      match l' with
      | [] -> (previous, first) :: acc
      | x :: tail -> 
        let acc' = (previous, x) :: acc
        pairwiseWithCyclic' first x acc' tail
    match l with
    | [] -> []
    | h :: tail ->
      pairwiseWithCyclic' h h [] tail
      
  let pairwiseWithCyclic a = pairwiseWithCyclic_Reversed a |> List.rev

  let yInterp nbs newX =
    nbs.y_lower + nbs.dy_dx * (newX - nbs.x_lower)

  let truncate nbs max_x =
    if max_x >= nbs.x_upper then
      failwith <| sprintf "Bad normalization %f >= max, elt=%A" max_x nbs
    else
      { x_lower = nbs.x_lower; x_upper = max_x; 
        y_lower = nbs.y_lower; y_upper = yInterp nbs max_x;
        dy_dx = nbs.dy_dx }

  let split nbs mid_x =
    if mid_x >= nbs.x_upper || mid_x <= nbs.x_lower then
      failwith <| sprintf "Bad normalization %f outside bounds, elt=%A" mid_x nbs
    else
      let mid_y = yInterp nbs mid_x
      let left=
        { x_lower = nbs.x_lower; x_upper = mid_x; 
          y_lower = nbs.x_lower; y_upper = mid_y;
        dy_dx = nbs.dy_dx }
      let right =
        { x_lower = mid_x; x_upper = nbs.x_upper; 
          y_lower = mid_y; y_upper = nbs.y_upper;
        dy_dx = nbs.dy_dx }
      (left, right)

  let insertNormalizedSection sortedList inputElt =
    let rec seekForLeft acc list currElt =
      match list with
      | [] -> [currElt]
      | elt :: tail ->
        if elt.x_upper < currElt.x_lower then
          //             |l...r|
          //   |x.l x.r|
          seekForLeft (elt :: acc) tail currElt
        else
          // e.x_u >= c.x_l
          if elt.x_upper < currElt.x_upper then
            //         |l ...  r|      (currElt)
            //   |x.l   x.r|           (elt)
            let y_interp_left = yInterp elt currElt.x_lower
            let y_interp_right = yInterp currElt elt.x_upper
            if y_interp_left < currElt.y_lower then
              if elt.y_upper < y_interp_right then
                let first = truncate elt currElt.x_lower
                let (second, rem) = split currElt elt.x_upper
                seekForLeft (second :: first :: acc) tail rem
              else
                let dy0 = currElt.y_lower - y_interp_left // >0
                let d_dy_dx = currElt.dy_dx - elt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = currElt.x_lower + dx
                let (old_ab, old_c) = split elt intercept
                let old_a = truncate old_ab currElt.x_lower

                let (new_b, new_c) = split currElt intercept
                let (_, rem) = split new_c elt.x_upper

                //         new_b    old_c
                //         old_b         new_c
                //  old_a
                //  
                seekForLeft (old_c :: new_b :: old_a :: acc) tail rem
            else //i.e. y_interp_left >= currElt.y_lower 
              if elt.y_upper < y_interp_right then
                //               new_c new_d
                //               old_c
                //         old_b         
                //         new_b
                //  old_a
                //  
                let dy0 = currElt.y_lower - y_interp_left // <0
                let d_dy_dx = currElt.dy_dx - elt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = currElt.x_lower + dx
                let old_ab = truncate elt currElt.x_lower
                let (_, new_cd) = split currElt intercept
                let (new_c, rem) = split new_cd elt.x_upper
                seekForLeft (new_c :: old_ab :: acc) tail rem
              else
                
                //  old_a old_b olc_c
                //        new_b new_c new_d
                let (_, rem) = split currElt elt.x_upper
                seekForLeft (elt :: acc) tail rem
          else // elt.x_upper >= currElt.x_upper 
            //         |l ...  r|
            //     |x.l          x.r|
            let y_interp_left = yInterp elt currElt.x_lower
            let y_interp_right = yInterp elt currElt.x_upper
            if y_interp_left < currElt.y_lower then
              if y_interp_right < currElt.y_upper then
                let (first, second) = split elt currElt.x_lower
                let (_, rem) = split second currElt.x_upper
                rem :: second :: first :: acc
              else
                let dy0 = currElt.y_lower - y_interp_left // >0
                let d_dy_dx = currElt.dy_dx - elt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = currElt.x_lower + dx
                let (old_ab, old_c) = split elt intercept
                let old_a = truncate old_ab currElt.x_lower

                let (new_b, _) = split currElt intercept
                //         new_b         old_c
                //         old_b   new_c
                //  old_a
                //  
                old_c :: new_b :: old_a :: acc
            else
              if y_interp_right < currElt.y_upper then
                //               new_c old_d
                //               old_c
                //         old_b         
                //         new_b
                //  old_a
                let dy0 = currElt.y_lower - y_interp_left // <0
                let d_dy_dx = currElt.dy_dx - elt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = currElt.x_lower + dx
                let (old_ab, old_cd) = split elt currElt.x_lower
                let (_, new_c) = split currElt intercept
                let (_, old_d) = split old_cd intercept
                old_d :: new_c :: old_ab :: acc
              else
                currElt :: acc
    seekForLeft [] sortedList inputElt |> List.rev

  let makeBoundaryData points centroid referenceCart : ClusterBoundary = 
    let normBoundary = 
      points
      |> pairwiseWithCyclic_Reversed
      |> List.fold( 
        fun s (l,r) ->
          let l' =
            if r.argument <= l.argument then
              l
            else
              { argument = l.argument + (2.0 * System.Math.PI); pt = l.pt; inUrl=l.inUrl; outUrl = l.outUrl; radius = l.radius }
          insertNormalizedSection s (makeNBS_LR l' r)) []
      |> Array.ofList

    { pts = points; hub = centroid; ref = referenceCart; normalizedBoundary = normBoundary }

  let lookupBoundary boundaryArr argument =
    let size = Array.length boundaryArr
    let arg' =
      if Array.isEmpty boundaryArr then
        failwith "Empty Boundary"
      elif argument < boundaryArr.[0].x_lower then
        argument + (2.0 * System.Math.PI)
      elif argument < boundaryArr.[size-1].x_upper then
        failwith "Boundary missing some bounds"
      else
        argument
    let rec binSearchBdry (boundary : NormalizedBoundarySection array) lower uppern1 current a =
      if a >= boundary.[current].x_lower then
        if a <= boundary.[current].x_upper then
          boundary.[current]
        else
          if current + 1 = uppern1 then
            boundary.[current]
          else
            binSearchBdry boundary current uppern1 ((current + uppern1) / 2) a
      else
        if lower + 1 = current then
          boundary.[current]
        else
          binSearchBdry boundary lower current ((lower + current) / 2) a
    binSearchBdry boundaryArr 0 size (size/2) arg'

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

  

    // This function (2022-07-03) had some bug
    // Most likely the issue is that the initial state is not well handled in the multi-valency case
    // Update -- it appears to be fixed by duplicating the (only) external edge as last and first connection
    //      ... in the valency=1 case

    let rec foldClusterBoundary' verbose initialPair previousPoint listSoFar vertexUrl =
      if verbose then
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
        let withBearings' = 
          getBearings hub samplePoint (fst >> urlToCartesian ts 1.0) neighboursAndIsInternal
          |> List.sortBy snd
        // Some kind of edge case here.... maybe we should duplicate the samplePoint as 0 AND tPI if the valency is only 1 (i.e. 2).
        let countExternal = neighboursAndIsInternal |> List.filter(snd) |> List.length
        let withBearings = 
          if countExternal = 1 then
            withBearings' @ [List.head withBearings']
          else
            withBearings'
        let [(_, consistencyBearing)] = getBearings hub samplePoint (urlToCartesian ts 1.0) [previousPoint]
        let isInside = fun ((_, a), _) -> a = true
        if verbose  then
          printfn "Check %f" consistencyBearing
          withBearings
          |> List.iter(fun ((vtx, inflag), b) ->
            printfn "%f (%b): %s " b inflag <| vtxStr vtx)
        let outsideAction ((outUrl, _), _) =
          makeBoundaryPoint hub vertexUrl outUrl
        let insideAction acc ((inUrl, _), _) =
          foldClusterBoundary' verbose i' vertexUrl acc inUrl
        accumulateSomethingButSkipHeadIfTrue listSoFar insideAction outsideAction isInside withBearings

    let points = foldClusterBoundary' false None referencePoint [] firstUrl
    makeBoundaryData points centroid referenceCart

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
    |> normalize
    
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

  let getLocalCoordinates cluster localPoint =
    let b = makeBearinger cluster.hub cluster.ref
    let th = b localPoint
    let r = modulus (localPoint - cluster.hub)
    let s = lookupBoundary cluster.normalizedBoundary th
    let r_actual = yInterp s th
    (r / r_actual, th)