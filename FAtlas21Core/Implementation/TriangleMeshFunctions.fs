namespace FAtlas


open CoordTypes
open SphereMeshTypes
open TriangleTypes
open TriangleMeshTypes
open CoordFunctions


module TriangleMeshFunctions =

  let vertexConverters n = 
    let nn1 = ((n + 1) * n) / 2
    let mutable i = -1
    let mutable j = 0
    let mutable thisN = n
    let verticesArray = 
      Array.init(nn1) (fun k ->
        i <- i + 1
        if i >= thisN then
          thisN <- thisN - 1
          i <- 0
          j <- j + 1
        else
          ()
        ((i,j), k))

    let vertices2dTo1d =
      verticesArray
      |> Map.ofArray

    let vertices1dTo2d =
      verticesArray
      |> Array.map(fun (i,j) -> (j,i))
      |> Map.ofArray
    { nplusOne = n; vertices2dTo1d = vertices2dTo1d; vertices1dTo2d = vertices1dTo2d }
  
  
  let makeKeyAB (a:CoordinateKeys) (b:CoordinateKeys) =
    let aweigt = a |> List.sumBy snd
    let bweight = b |> List.sumBy snd

    let exclusiveA = a |> List.exists(fun (ka,_) -> List.exists(fun (kb,_) -> ka = kb) b |> not)
    let exclusiveB = b |> List.exists(fun (ka,_) -> List.exists(fun (kb,_) -> ka = kb) a |> not)

    let x = 
      if exclusiveA && exclusiveB then
        2
      else 
        1
      
    let (am, bm) =
      if aweigt > bweight then
        (1, aweigt / bweight)
      else
        (bweight / aweigt, 1)
    let rec mergeKeys' acc a b =
      match (a,b) with
      | (_,[]) -> a @ acc
      | ([],_) -> b @ acc
      | ((ac1, ab1) :: a2, (bc1, bb1) :: b2) ->
        if ac1 = bc1 then
          let acc' = (ac1, (am * ab1) + (bm * bb1)) :: acc
          mergeKeys' acc' a2 b2
        elif ac1 < bc1 then
          let acc' = (ac1, am * ab1) :: acc  
          mergeKeys' acc' a2 b
        else
          let acc' = (bc1, bm * bb1) :: acc 
          mergeKeys' acc' a b2
    mergeKeys' [] a b |> List.rev

  //let makeKeyAB keyA keyB= 
  //  let (first, second) =
  //    let lA = List.length(keyA)
  //    if lA > 2 then
  //      (keyA, keyB)
  //    else
  //      if lA > List.length(keyB) then
  //        (keyA, keyB)
  //      else 
  //        (keyA, keyB)
  //  first 
  //  |> List.map(fun (c,i) ->
  //    second 
  //    |> List.choose(fun (c',i') -> 
  //      if c' = c then Some(i') else None) |> List.sum |> fun i'' -> (c, i + i''))
  //  |> List.filter(fun (c, i) -> i > 0)
  //  |> List.sortBy(fst)

  let divideNoLookup getKey pointInterpolator pointA pointB = 
    let keyA = getKey pointA
    let keyB = getKey pointB
    makeKeyAB keyA keyB |> pointInterpolator pointA pointB

  let divide getKey pointInterpolator lookup pointA pointB = 
    let newKey = makeKeyAB (getKey pointA) (getKey pointB)
    let nkl = List.length(newKey)
    if nkl = 2 then
      lookup |> Map.tryFind(newKey) |> fun m -> 
        match m with 
        | Some(ex) -> ([], ex)
        | None -> 
          let res = pointInterpolator pointA pointB newKey
          let lookup' = lookup |> Map.add(newKey) res
          ((newKey, res) :: [], res)
    else
      let res = pointInterpolator pointA pointB newKey
      ([], res)

  let divideAlongRow getKey pointInterpolator lookup row i =
    let l = Array.length(row)

    let creationLookup h = 
        divide getKey pointInterpolator lookup row.[h] row.[h+1]

    if i = 0 then
      let l' = l / 2
      let newPoints = Array.init(l - 1) creationLookup
      let newKeys = newPoints |> List.ofArray |> List.collect(fst)
      let creation' j =
        let h = j / 2
        if j % 2 = 0 then
          row.[h]
        else
          newPoints.[h] |> snd
      (newKeys, Array.init(2 * l - 1) creation')
    else
      let creation j =
        let h = j / 2
        if j % 2 = 0 then
          row.[h]
        else
          divideNoLookup getKey pointInterpolator row.[h] row.[h+1]
      ([], Array.init(2 * l - 1) creation)

  let divideBetweenRows getKey pointInterpolator lookup rowA rowB =
    // NB len(rowA) = 1 + len(rowB)
    //        A = 1 + B
    // compute pairs (i,j) where i=j or i=j+1
    // special case (i,j) = (0,0) 
    //          and (i,j) = (B, 1-B)
    let B = Array.length(rowB)
    let A = Array.length(rowA)
    let (e00, a00) = divide getKey pointInterpolator lookup rowA.[0] rowB.[0]
    let (eb1b, ab1b) = divide getKey pointInterpolator lookup rowA.[B] rowB.[B - 1]
    let creation i =
      match i with
      | 0 -> a00
      | x when x = ((2*B) - 1) -> ab1b
      | y ->
        let b = y / 2
        let a = y - b
        divideNoLookup getKey pointInterpolator rowA.[a] rowB.[b]
      
    let newKeys = e00 @ eb1b 
    (newKeys, Array.init(2 * B) creation)

  let divideWholeTriangle getPoints makeEmpty getKey pointInterpolator lookup triangle =
    let basePoints = getPoints triangle
    let N = basePoints|> Array.length
    let N' = (2 * (N - 1))
    let newTriangle = makeEmpty triangle N'
    let newPoints = getPoints newTriangle
    let newPoints' = 
      basePoints
      |> Array.mapi(fun i row ->
        let (pts, row') = divideAlongRow getKey pointInterpolator lookup row i
        newPoints.[2 * i] <- row'
        let pts' =
          if i > 0 then
            let (pts'', row'') = divideBetweenRows getKey pointInterpolator lookup basePoints.[i-1] row
            newPoints.[(2 * i) - 1] <- row''
            pts''
          else
            []
        pts @ pts')
    let newKeys = newPoints' |> Seq.ofArray |> Seq.collect(id) 
    let lookup' = newKeys |> Seq.fold(fun m (k, t) -> m |> Map.add(k) t) lookup
    printfn "Old length %i %i" basePoints.Length basePoints.[0].Length
    printfn "New length %i %i" newPoints.Length newPoints.[0].Length
    (lookup', newTriangle)

  let singleDivideTriangleSet initCache makeEmpty getKey interpolator triangleSet =
    let output = { triangleSet with triangles = Array.copy triangleSet.triangles } 
    let getPoints (ts : SingleTriangle<'A>) = (ts.points)
    let foldFn (cache, i) triangle = 
      let (cache', triangle') = divideWholeTriangle getPoints makeEmpty getKey interpolator cache triangle
      output.triangles.[i] <- triangle'
      (cache', i+1)
    triangleSet.triangles |> Array.fold foldFn (initCache, 0) |> ignore
    output

  let createIcosahedronSet (f : (char*int) list -> Cartesian -> 'A) = 
    let asThree = function 
      | [i;j;k] -> (i,j,k)
      | x -> failwith <| sprintf "Not 3 elements in %A" x
    let idxToKey i = char (65 + i)
    let lookupArr = 
      icosahedronVertices 
      |> Array.mapi(fun i c -> (idxToKey i, c))
      |> Map.ofArray
    let scale = 0
    let triangles =
      icosahedronFaceIndices
      |> Array.map(
        fun ijk ->
          let ints = ijk |> List.map(int)
          let maker i =
            let icart = icosahedronVertices.[i]
            let ikey = [(idxToKey i, 1)]
            f ikey icart
          let keys = ints |> List.map(idxToKey)
          let points = ints |> List.map(maker)
          makeABC (asThree keys) (asThree points)
          )
    let frame = { frameMap = lookupArr; scaleFactor = scale }
    triangleSet frame triangles

  
  let getInnerPoints i j =
    if i = 0 then
      [(1, j-1); (1, j)]
    elif j = 0 then
      [(i-1,1); (i, 1)]
    else
      // i + j == N
      [(i,j-1); (i-1,j)]

  let getOtherTriangleForEdge ts i_from key =
    Map.find key ts.trianglesByEdge
    |> fun ed -> ed.duplicateEdges
    |> List.filter(fun (i_to, _) -> i_from <> i_to)
    |> List.head

  let ijFromOV N ov = 
    match ov with
    | OV_A -> (0,0)
    | OV_B -> (N,0)
    | OV_C -> (0,N)

  let getCanonicalVertex (ts : TriangleSet<'A>) N key =
    let data = Map.find key ts.trianglesByVertex
    let (t, ov) = data.canonical
    let (i,j) = ijFromOV N ov
    { t = t; i = i; j = j }

  let getCanonEdgeKey (ts : TriangleSet<'A>) (idx : int, orientation : OrientedEdge) =
    let (a,b,c) = extractKey ts.triangles.[idx]
    match orientation with
    | OE_AB -> (a,b)
    | OE_AC -> (a,c)
    | OE_CA -> (c,a)
    | OE_CB -> (c,b)
    | OE_BA -> (b,a)    
    | OE_BC -> (b,c)

  let isInverted ts (idx,oe) desiredKey = 
    let ck = getCanonEdgeKey ts (idx, oe)
    if ck = desiredKey then
      false
    else
      let (d1,d2) = desiredKey
      if ck = (d2, d1) then
        true
      else
        let (c1,c2) = ck
        failwith <| sprintf "Two keys, (%c,%c) and (%c, %c) are unrelated" d1 d2 c1 c2

  let getIthVertexAlongEdge (ts : TriangleSet<'A>) N (idx : int, orientation : OrientedEdge) i =
    if (i = 0) || (i = N) then
      let (a,b,c) = extractKey ts.triangles.[idx]
      let key = 
        if (i = 0) then
          match orientation with
          | OE_AB -> //AB
            a
          | OE_BC -> //BC
            b
          | OE_CA -> //CA
            c
          | OE_BA -> //BA
            b
          | OE_CB -> //CB
            c
          | OE_AC -> //AC
            a
        else 
          match orientation with
          | OE_AB -> //AB
            b
          | OE_BC -> //BC
            c
          | OE_CA -> //CA
            a
          | OE_BA -> //BA
            a
          | OE_CB -> //CB
            b
          | OE_AC -> //AC
            c
      let canon = Map.find key ts.trianglesByVertex 
      canon.canonical
      |> fun (idx, ov) -> (idx, ijFromOV N ov)
    else
      let ij =
        match orientation with
        | OE_AB -> //AB
          (i, 0)
        | OE_BC -> //BC
          (N - i, i)
        | OE_CA -> //CA
          (0, N-i)
        | OE_BA -> //BA
          (N-i, 0)
        | OE_CB -> //CB
          (i, N-i)
        | OE_AC -> //AC
          (0, i)
      (idx, (ij))

  let getAllNeighboursOfVertex (ts : TriangleSet<'A>) (hub : char) (idxs : int list) =
    // Need to de-dupe
    // Can group edge refs
    // for given edge, use vertex from lower index triangle?
    let duplicated = 
      idxs
      |> List.collect(fun i ->
        let t = ts.triangles.[i]
        let N = sideLength t.scale - 1
        let (a,b,c) = extractKey t
        let ab = if a < b then (a,b) else (b,a)
        let ac = if a < c then (a,c) else (c,a)
        let bc = if b < c then (b,c) else (c,b)
        match hub with
        | h when a = h -> 
          let left = (ab, i, {t=i; i=1;j=0})
          let right = (ac, i, {t=i; i=0;j=1})
          [left; right]
        | h when b = h ->
          let left = (ab, i, {t=i; i=N-1;j=0})
          let right = (bc, i, {t=i; i=N-1;j=1})
          [left; right]
        | h when c = h ->
          let left = (ac, i, {t=i; i=0;j=N-1})
          let right = (bc, i, {t=i; i=1;j=N-1})
          [left; right]
        | _ -> failwith <| sprintf "Bad vertex key '%c'" hub)
    let deduped =
      duplicated 
      |> List.groupBy(fun (pr,_,_) -> pr)
      |> List.map(
        fun (pr, elts) ->
          elts 
          |> List.sortBy(fun (_,i,_) -> i)
          |> List.head
          |> fun (_,_,u) -> u
      )
    deduped
        

  let getEdgeInterior N (oe : OrientedEdge) idx =
    if idx = 0 || idx = N then
      failwith <| sprintf "Logic Error, getEdgeInterior should be not be called on corners i='%i'" idx
    else
      match oe with
      | OE_AB ->
        [(idx-1,1);(idx,1)]
      | OE_AC ->
        [(1, idx-1);(1,idx)]
      | OE_BA ->
        [((N-idx)-1,1);(N-idx,1)]
      | OE_BC ->
        let (i',j') = (N-idx, idx)
        [(i',j'-1);(i'-1,j')]
      | OE_CA->
        [(1,(N-idx)-1);(1,N-idx)]
      | OE_CB->
        let (i',j') = (idx, N-idx)
        [(i',j'-1);(i'-1,j')]
  
  let getOtherEdgeNeighbours ts N from_t key idx =
    
    let edge = Map.find key ts.trianglesByEdge
    let along = [getIthVertexAlongEdge ts N edge.canonical (idx-1); getIthVertexAlongEdge ts N edge.canonical (idx+1)]
    let (o_t, o_o) = getOtherTriangleForEdge ts from_t key
    let far = getEdgeInterior N o_o idx |> List.map(fun x -> (o_t, x))
    let non_local = 
      along @ far 
      |> List.map(fun (t,(i,j)) -> { t = t; i = i; j = j})
    non_local

  let getCanonicalEdgeElements  (ts : TriangleSet<'A>) N key idx = 
    let edge = Map.find key ts.trianglesByEdge
    let idx' = 
      if isInverted ts edge.canonical key then
        N - idx
      else
        idx
    getIthVertexAlongEdge ts N edge.canonical idx'
    |> fun (t,(i,j)) -> { t = t; i=i; j=j}

  let normalizeElement (ts : TriangleSet<'A>) N (a,b,c) elt =
    if elt.i = 0 then
      if elt.j = 0 then
        getCanonicalVertex ts N a
      elif elt.j = N then
        getCanonicalVertex ts N c
      else
        getCanonicalEdgeElements ts N (a,c) elt.j
    elif elt.i = N then
      getCanonicalVertex ts N b
    elif elt.j = 0 then
      getCanonicalEdgeElements ts N (a,b) elt.i
    elif elt.i + elt.j = N then
      getCanonicalEdgeElements ts N (b,c) elt.j
    else
      elt

  let getVertexNeighbours (ts : TriangleSet<'A>) (st: SingleTriangle<'A>) (t : int) (i,j) = 
    // points
    // "A" [0][0]    ->        -> "C" [0][N]
    //     [1][0]                [1][N-1]
    //  v
    //     [N-1][0] [N-1][1]
    // "B" [N][0]

    // corner vertices A,B,C exist as part of 5 triangles (in icosahedron case) and have 5 neighbours.
    // edge vertices along AB / BC / CA exist as part of 2 triangles. (4 neighbours in each triangle, of which 2 are repeated)
    // inner vertices have 6 neighbours

    // general case the connections are:

    //     ---        [i-1][j]  [i-1][j+1]
    //     [i][j-1]      XX     [i][j+1]
    //     [i+1][j-1] [i+1][j]     ---

    let N = (Array.length st.points) - 1
    if N = 1 then
      failwith "nyi"
    else
      let (a,b,c) = extractKey st
      if i = 0 && j = 0 then
        // Corner A
        Map.find a ts.trianglesByVertex
        |> fun x -> List.map fst x.facesTouched
        |> getAllNeighboursOfVertex ts a
      elif i = 0 && j = N then
        // Corner C
        Map.find c ts.trianglesByVertex
        |> fun x -> List.map fst x.facesTouched
        |> getAllNeighboursOfVertex ts c 
      elif i = N && j = 0 then
        // Corner B
        Map.find b ts.trianglesByVertex
        |> fun x -> List.map fst x.facesTouched
        |> getAllNeighboursOfVertex ts b
      else
        if i = 0 then
          // Edge AC
          let local_elts = [ 
            { t = t; i = 1; j = j}; 
            { t = t; i = 1; j = j-1} ]
          let idx = j
          let key = (a,c)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          non_local @ local_elts
        elif j = 0 then
          // Edge AB
          let local_elts = [ 
            { t = t; i = i-1; j = 1}; 
            { t = t; i = i; j = 1} ]
          let idx = i
          let key = (a,b)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          non_local @ local_elts
        elif (i+j) = N then
          // Edge BC
          let local_elts = [ 
            { t = t; i = i-1; j = j}; 
            { t = t; i = i; j = j-1} ]
          let idx = j
          let key = (b,c)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          non_local @ local_elts
        else
          // 6 inner vertices... however still chance some of them are "non-canonical"...
          // Maybe should have a global map (edge) to canonical triangle
          
          let local_elts = [ 
            { t = t; i = i+1; j = j-1}; 
            { t = t; i = i-1; j = j+1}; 
            { t = t; i = i+1; j = j}; 
            { t = t; i = i; j = j+1}; 
            { t = t; i = i-1; j = j}; 
            { t = t; i = i; j = j-1} ]
          local_elts
          |> List.map(normalizeElement ts N (a,b,c))

  let uniformCoordinate (rng : System.Random) (oneDtoTwoD : Map<int,int*int>) (ts : TriangleSet<'A>)  =
    let t = rng.Next(0,20)
    let N = (Array.length ts.triangles.[t].points) - 1
    let ij = rng.Next(0, (N+1)*(N+2)/2)
    let (i,j) = Map.find ij oneDtoTwoD
    { t = t; i = i; j = j }

  
  let getVertexNeighboursFromUrl (ts : TriangleSet<'A>) (url : VertexUrl) =
    let st = ts.triangles.[url.t]
    let r = getVertexNeighbours ts st url.t (url.i, url.j)
    let x =
      if List.exists(fun u -> u.i + u.j > 16) r then
        2
      else
        1
    r

  let urlToKey (ts : TriangleSet<KeyedPoint<'A>>) url = 
    let t = ts.triangles.[url.t]
    let p = t.points.[url.i].[url.j]
    p.key
          
