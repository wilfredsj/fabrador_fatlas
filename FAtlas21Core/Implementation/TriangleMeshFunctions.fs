namespace FAtlas


open CoordTypes
//open SphereMeshTypes
open TriangleTypes
open TriangleMeshTypes
open CoordFunctions


module TriangleMeshFunctions =

  let triConverter n = 
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

    
  let hexConverter_BadDual (triConverters : int -> VertexConverters) n = 
    // Here for posterity.
    // This maps edges to vertices, 
    // It should map faces to vertices
    if n <= 1 then failwith <| sprintf "HexConverter only good for n >= 2, n input = %i" n
    else
      let triConv = triConverters (max (n-3) 1)
      let nn1 = ((n + 1) * n) / 2
      // Can group m into:
      // A:    3 vertices (w 2 edges each) ,           m=6          k = 9
      // B:    3*(n-2) edge vertices (w 4 edges each)  m=12n-24   , k = 15n-30
      // C:    nn1 - (3*(n-1)) inner vertices w 6 edges each   
      //                                               m=6nn1 - 18n + 18, 
      //                                                            k = 7nn1 - 21n + 21
      let kTotal = (7 * nn1 - (6 * n))

      // n=1 => kTotal = 1 ... ?
      // n=2 => kTotal = 21 - 12 = 9.... 3 corners * (2+1) edges
      // n=3 => kTotal = 42 - 18 = 24 ... 9 + 3*(4+1)     
      // n=4 => kTotal = 70 - 24 = 46 ... 9 + 6*5 + 1*(6+1)
      // n=5 => kTotal = 105 -30 = 75 ... 9 + 9*5 + 3*7 

      let kThresh2 = ((15* n) - 30) + 9
      // n=1 => kThresh2 = -6
      // n=2 => kThresh2 = 9
      // n=3 => kThresh3 = 24
      let verticesArray = 
        Array.init kTotal (fun k ->
          if k < 9 then
            (k,
              match k with
              | 0 -> ((0,0),(0,0))
              | 1 -> ((0,0),(1,0))
              | 2 -> ((0,0),(0,1))
              | 3 -> ((n-1,0),(n-1,0))
              | 4 -> ((n-1,0),(n-2,1))
              | 5 -> ((n-1,0),(n-2,0))
              | 6 -> ((0,n-1),(0,n-1))
              | 7 -> ((0,n-1),(0,n-2))
              | 8 -> ((0,n-1),(1,n-2))
              | _ -> failwith <| sprintf "Logic error k (%i) > 8" k)
          elif k < kThresh2 then
            let kRes = k - 9
            let numPerEdge = (5 * n) - 10
            let edgeNum = kRes / numPerEdge
            let edgeEltNum = kRes % numPerEdge
            let vertexNum = edgeEltNum / 5
            let vertexDirNum = edgeEltNum % 5
            let dirs = 
              match edgeNum with
              | 0 -> [| (1,0); (0,1); (-1, 1); (-1,0) |]
              | 1 -> [| (-1, 1); (-1,0); (0,-1); (1, -1)|]
              | 2 -> [|  (0,-1); (1, -1); (1,0); (0,1) |]
              | _ -> failwith <| sprintf "Logic error edgenum (%i) > 3" edgeNum
            let (base_i, base_j) =
              match edgeNum with
              | 0 -> (0,0)
              | 1 -> (n-1,0)
              | 2 -> (0,n-1)
              | _ -> failwith <| sprintf "Logic error edgenum %i > 3" edgeNum
            let (edge_di, edge_dj) = 
              dirs.[0]
            let (thisBase_i, thisBase_j)= (base_i + (vertexNum + 1) * edge_di, base_j + (vertexNum + 1) * edge_dj)
            let (other_i, other_j) =
              match vertexDirNum with
              | 0 -> (thisBase_i, thisBase_j)
              | _ -> (thisBase_i + fst dirs.[vertexDirNum-1], thisBase_j + snd dirs.[vertexDirNum-1])
            (k,
              ((thisBase_i, thisBase_j),
               (other_i, other_j)))
          else
            let kRes = k - kThresh2
            let numPerVertex = 7
            let vertexNum = kRes / numPerVertex
            let vertexDirNum = kRes % numPerVertex
            // Claim is that n(n+1)/2 - 3*(n-1) = (n-3)*(n-2)/2
            //               1/2 * n^2 - 5/2 n + 3  = 1/2 n^2 - 5/2 n + 3
            //         .
            //        . .
            //       . * .
            //      . * * .
            //     . . . . . 

            let (innerBase_i, innerBase_j) = triConv.vertices1dTo2d.[vertexNum]
            // CAUTION:
            let (thisBase_i, thisBase_j) = (innerBase_i + 1, innerBase_j + 1)
          
            let dirs = [| (1,0); (0,1); (-1, 1); (-1,0); (0,-1); (1, -1)|]
          
            let (other_i, other_j) =
              match vertexDirNum with
              | 0 -> (thisBase_i, thisBase_j)
              | _ -> (thisBase_i + fst dirs.[vertexDirNum-1], thisBase_j + snd dirs.[vertexDirNum-1])
            
            (k,
              ((thisBase_i, thisBase_j),
                (other_i, other_j))))
              
      let vertices1dTo2dPhysical = verticesArray |> Map.ofArray
    
      let vertices1dTo2dLogical = 
        verticesArray 
        |> Array.map(fun (k,(ij1, _)) -> (k, ij1))
        |> Map.ofArray

      let vertices2dPhysicalTo1d =
        verticesArray
        |> Array.map(fun (i,j) -> (j,i))
        |> Map.ofArray

      
      let dupeCheck =
        verticesArray
        |> Array.map(fun (i,j) -> (j,i))
        |> Array.groupBy(fst)
        |> Array.map(fun (i, arr) -> (i, Array.length arr, arr))
        |> Array.filter(fun (i,l,a) -> l > 1)
      { 
        nplusOne_bd = n; 
        numEntries = Array.length verticesArray; 
        vertices1dTo2dPhysical = vertices1dTo2dPhysical; 
        vertices1dTo2dLogical = vertices1dTo2dLogical; 
        vertices2dPhysicalTo1d = vertices2dPhysicalTo1d 
      }
  
  let hexConverter (triConverters : int -> VertexConverters) n : HexVertexConverters = 
      // This maps faces to vertices
      if n <= 1 then failwith <| sprintf "HexConverter only good for n >= 2, n input = %i" n
      else
        let triConv = triConverters (max (n-3) 1)
        let nn1 = ((n + 1) * n) / 2
        // Can group m into:
        // A:    3 vertices (w 3 edges each) ,           m=6          k = 12
        // B:    3*(n-2) edge vertices (w 5 edges each)  m=12n-24   , k = 18n-36
        // C:    nn1 - (3*(n-1)) inner vertices w 6 edges each   
        //                                               m=6nn1 - 18n + 18, 
        //                                                            k = 7nn1 - 21n + 21
        let kTotal = (7 * nn1 - ((3 * n) + 3))

        // n=1 => kTotal = 3 ... ?
        // n=2 => kTotal = 21 - 9  = 12 ... 3 corners * (3+1) edges
        // n=3 => kTotal = 42 - 12 = 30 ... 12 + 3*(5+1)     
        // n=4 => kTotal = 70 - 15 = 55 ... 12 + 6*6 + 1*(6+1)
        // n=5 => kTotal = 105 -18 = 87 ... 12 + 9*6 + 3*7 

        let kThresh2 = ((18 * n) - 36) + 12
        // n=1 => kThresh2 = -24
        // n=2 => kThresh2 = 12
        // n=3 => kThresh3 = 30
        let verticesArray = 
          Array.init kTotal (fun k ->
            if k < 12 then
              (k,
                match k with
                | 0 -> ((0,0),(0,0),(-1,-1))
                | 1 -> ((0,0),(1,0),(-1,-1))
                | 2 -> ((0,0),(1,0),( 0, 1))
                | 3 -> ((0,0),(0,1),(-1,-1))
                | 4 -> ((n-1,0),(n-1,0),(-1,-1))
                | 5 -> ((n-1,0),(n-2,1),(-1,-1))
                | 6 -> ((n-1,0),(n-2,1),(n-2,0))
                | 7 -> ((n-1,0),(n-2,0),(-1,-1))
                | 8 -> ((0,n-1),(0,n-1),(-1,-1))
                | 9 -> ((0,n-1),(0,n-2),(-1,-1))
                | 10 -> ((0,n-1),(0,n-2),(1,n-2))
                | 11 -> ((0,n-1),(1,n-2),(-1,-1))
                | _ -> failwith <| sprintf "Logic error k (%i) > 11" k)
            elif k < kThresh2 then
              let kRes = k - 12
              let numPerEdge = (6 * n) - 12
              let edgeNum = kRes / numPerEdge
              let edgeEltNum = kRes % numPerEdge
              let vertexNum = edgeEltNum / 6
              let vertexDirNum = edgeEltNum % 6
              
              // Recap... egdes map to 2. Faces map to 3.
              // This will be: [center, edge, face, face, face, edge]
              //               [center, [0], [0][1], [1][2], [2][3], [3]]
              let dirs = 
                match edgeNum with
                | 0 -> [| (1,0); (0,1); (-1, 1); (-1,0) |]
                | 1 -> [| (-1, 1); (-1,0); (0,-1); (1, -1)|]
                | 2 -> [|  (0,-1); (1, -1); (1,0); (0,1) |]
                | _ -> failwith <| sprintf "Logic error edgenum (%i) > 3" edgeNum
              let (base_i, base_j) =
                match edgeNum with
                | 0 -> (0,0)
                | 1 -> (n-1,0)
                | 2 -> (0,n-1)
                | _ -> failwith <| sprintf "Logic error edgenum %i > 3" edgeNum
              let (edge_di, edge_dj) = 
                dirs.[0]
              let (thisBase_i, thisBase_j)= (base_i + (vertexNum + 1) * edge_di, base_j + (vertexNum + 1) * edge_dj)
              let (other_i, other_j, next_i, next_j) =
                match vertexDirNum with
                | 0 -> (thisBase_i, thisBase_j, -1, -1)
                | 1 -> (thisBase_i + fst dirs.[0], thisBase_j + snd dirs.[0], -1, -1)
                | 5 -> (thisBase_i + fst dirs.[3], thisBase_j + snd dirs.[3], -1, -1)
                | _ -> (thisBase_i + fst dirs.[vertexDirNum-2], thisBase_j + snd dirs.[vertexDirNum-2],
                        thisBase_i + fst dirs.[vertexDirNum-1], thisBase_j + snd dirs.[vertexDirNum-1])
              (k,
                ((thisBase_i, thisBase_j),
                 (other_i, other_j),
                 (next_i, next_j)))
            else
              let kRes = k - kThresh2
              let numPerVertex = 7
              let vertexNum = kRes / numPerVertex
              let vertexDirNum = kRes % numPerVertex
              let nextDirNum = 
                if vertexDirNum + 1 = numPerVertex then
                  1
                else
                  vertexDirNum + 1
                  
              // Claim is that n(n+1)/2 - 3*(n-1) = (n-3)*(n-2)/2
              //               1/2 * n^2 - 5/2 n + 3  = 1/2 n^2 - 5/2 n + 3
              //         .
              //        . .
              //       . * .
              //      . * * .
              //     . . . . . 

              let (innerBase_i, innerBase_j) = triConv.vertices1dTo2d.[vertexNum]
              // CAUTION:
              let (thisBase_i, thisBase_j) = (innerBase_i + 1, innerBase_j + 1)
            
              let dirs = [| (1,0); (0,1); (-1, 1); (-1,0); (0,-1); (1, -1)|]
            
              let (other_i, other_j, next_i, next_j) =
                match vertexDirNum with
                | 0 -> (thisBase_i, thisBase_j, -1, -1)
                | _ -> (thisBase_i + fst dirs.[vertexDirNum-1], thisBase_j + snd dirs.[vertexDirNum-1],
                        thisBase_i + fst dirs.[nextDirNum-1], thisBase_j + snd dirs.[nextDirNum-1])
              
              (k,
                ((thisBase_i, thisBase_j),
                  (other_i, other_j), (next_i, next_j))))
                
        let vertices1dTo2dPhysical = verticesArray |> Map.ofArray
      
        let vertices1dTo2dLogical = 
          verticesArray 
          |> Array.map(fun (k,(ij1, _, _)) -> (k, ij1))
          |> Map.ofArray

        let vertices2dPhysicalTo1d =
          verticesArray
          |> Array.map(fun (i,j) -> (j,i))
          |> Map.ofArray

        
        let dupeCheck =
          verticesArray
          |> Array.map(fun (i,j) -> (j,i))
          |> Array.groupBy(fst)
          |> Array.map(fun (i, arr) -> (i, Array.length arr, arr))
          |> Array.filter(fun (i,l,a) -> l > 1)
        { 
          nplusOne = n; 
          numEntries = Array.length verticesArray; 
          vertices1dTo2dPhysical = vertices1dTo2dPhysical; 
          vertices1dTo2dLogical = vertices1dTo2dLogical; 
          vertices2dPhysicalTo1d = vertices2dPhysicalTo1d 
        }
  
  let makeKeyAB (a:CoordinateKeys) (b:CoordinateKeys) =
    let aweigt = a |> List.sumBy snd
    let bweight = b |> List.sumBy snd

    //let exclusiveA = a |> List.exists(fun (ka,_) -> List.exists(fun (kb,_) -> ka = kb) b |> not)
    //let exclusiveB = b |> List.exists(fun (ka,_) -> List.exists(fun (kb,_) -> ka = kb) a |> not)

    //let x = 
    //  if exclusiveA && exclusiveB then
    //    2
    //  else 
    //    1
      
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

  let divideWholeTriangle getPoints getScale makeEmpty getKey pointInterpolatorIn lookup triangle =
    let basePoints = getPoints triangle
    let N = basePoints|> Array.length
    let N' = (2 * (N - 1))
    let newTriangle = makeEmpty triangle N'
    let scale = getScale triangle
    let pointInterpolator = pointInterpolatorIn scale
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
    (lookup', newTriangle)

  let singleDivideTriangleSet initCache makeEmpty getKey interpolator triangleSet =
    let output = { triangleSet with triangles = Array.copy triangleSet.triangles } 
    let getPoints (ts : SingleTriangle<'A>) = (ts.points)
    let getScale (ts : SingleTriangle<'A>) = (ts.scale)
    let foldFn (cache, i) triangle = 
      let (cache', triangle') = divideWholeTriangle getPoints getScale makeEmpty getKey interpolator cache triangle
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

  let getIthVertexAlongEdgeAsUrl ts N idxPair i =
    getIthVertexAlongEdge ts N idxPair i |> fun (t,(i,j)) -> { t = t; i = i; j = j}

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
  

  let getCanonicalEdgeElements  (ts : TriangleSet<'A>) N key idx = 
    let edge = Map.find key ts.trianglesByEdge
    let idx' = 
      if isInverted ts edge.canonical key then
        N - idx
      else
        idx
    getIthVertexAlongEdge ts N edge.canonical idx'
    |> fun (t,(i,j)) -> { t = t; i=i; j=j}
      
  let normalizeElement (ts : TriangleSet<'A>) elt =
    let st = ts.triangles.[elt.t]
    let N = dim st
    let (a,b,c) = extractKey st
    let normalizeElement' (ts : TriangleSet<'A>) N (a,b,c) elt =
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
    normalizeElement' ts N (a,b,c) elt
  
  let rec getEdgeInterior ts N t (oe : OrientedEdge) idx =
    if idx = 0 || idx = N then
      failwith <| sprintf "Logic Error, getEdgeInterior should be not be called on corners i='%i'" idx
    else
      let (a,b,c) = extractKey ts.triangles.[t]
      match oe with
      | OE_AB ->
        if idx = 1 then
          if N = 2 then
            [
              getCanonicalEdgeElements ts N (a,c) 1;
              getCanonicalEdgeElements ts N (c,b) 1
            ]
          else
            let norm = getCanonicalEdgeElements ts N (a,c) 1
            [ norm ;
              { t=t; i=idx; j=1 } ]
        elif idx = N-1 then
          let norm = getCanonicalEdgeElements ts N (c,b) (N-1)
          [ { t=t; i=idx-1; j=1 };
            norm]
        else
          [ { t=t; i=idx-1; j=1 };
            { t=t; i=idx; j=1 } ]
      | OE_AC ->
        if idx = 1 then
          if N = 2 then
            [
              getCanonicalEdgeElements ts N (a,b) 1;
              getCanonicalEdgeElements ts N (b,c) 1
            ]
          else
            let norm = getCanonicalEdgeElements ts N (a,b) 1
            [ { t = t; i = 1; j = idx}; 
              norm ]
        elif idx = (N-1) then
          let norm = getCanonicalEdgeElements ts N (b,c) (N-1)
          [  norm; 
            { t = t; i = 1; j = idx-1} ]
        else
          [ { t = t; i = 1; j = idx}; 
            { t = t; i = 1; j = idx-1} ]
      | OE_BA ->
        getEdgeInterior ts N t OE_AB (N - idx)
      | OE_CB ->
          let j' = N-idx
          if idx = 1 then 
            if N = 2 then
              [
                getCanonicalEdgeElements ts N (a,c) 1;
                getCanonicalEdgeElements ts N (a,b) 1
              ]
            else
              // Close to 'C'
              // e.g. [1][N-1] is close to 'C'
            
              let norm = getCanonicalEdgeElements ts N (a,c) (N-1)
        
              [ { t = t; i = idx; j = j'-1}; 
              norm ]
          elif idx = N-1 then  
            //Close to 'B', i.e. [N-1][1]
            let norm = getCanonicalEdgeElements ts N (a,b) (N-1)
            [ norm;
              { t = t; i = idx-1; j = j'} ]
          else
            [ { t = t; i = idx-1; j = j'}; 
              { t = t; i = idx; j = j'-1} ]
      | OE_CA->
        getEdgeInterior ts N t OE_AC (N - idx)
      | OE_BC->
        getEdgeInterior ts N t OE_CB (N - idx)

  let getOtherEdgeNeighbours ts N from_t key idx =
          
    let edge = Map.find key ts.trianglesByEdge
    let along = 
      [getIthVertexAlongEdgeAsUrl ts N edge.canonical (idx-1); getIthVertexAlongEdgeAsUrl ts N edge.canonical (idx+1)]
    let (o_t, o_o) = getOtherTriangleForEdge ts from_t key
    let far = getEdgeInterior ts N o_t o_o idx 
    let non_local = 
      along @ far 
    non_local

  let addCounterCyclicIntoMap (map : Map<OrientedEdge, 'A>) = 
    map
    |> Map.add (OE_BA, Map.find OE_AB map)
    |> Map.add (OE_AC, Map.find OE_CA map)
    |> Map.add (OE_CB, Map.find OE_BC map)

  let getAllOtherTriangles (ts : TriangleSet<'A>) (t : int) = 
    let st = ts.triangles.[t]
    let N = dim st
    let (a,b,c) = extractKey st
    let edges = [
      (OE_AB,(a,b));
      (OE_BC,(b,c));
      (OE_CA,(c,a))
      ]
    edges
    |> List.map(fun (oe, oe_key) -> 
      let (o_t, o_o) = getOtherTriangleForEdge ts t oe_key
      (oe, o_t))
    |> Map.ofList
    |> addCounterCyclicIntoMap

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

    let N = dim st
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
          let local_elts = 
            if j = 1 then
              if N = 2 then
                [
                getCanonicalEdgeElements ts N (a,b) 1;
                getCanonicalEdgeElements ts N (b,c) 1
                ]
              else
              let norm = getCanonicalEdgeElements ts N (a,b) 1
              [ { t = t; i = 1; j = j}; 
                norm ]
            elif j = (N-1) then
              let norm = getCanonicalEdgeElements ts N (b,c) (N-1)
              [  norm; 
                { t = t; i = 1; j = j-1} ]
            else
              [ { t = t; i = 1; j = j}; 
                { t = t; i = 1; j = j-1} ]
          let idx = j
          let key = (a,c)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          let ret = non_local @ local_elts
          ret |> List.iter(fun x -> 
            let y = normalizeElement ts x
            if y <> x then 
              printfn "Non-Normalized Neighbour %s of %s" (vtxStr x) (vtxStr {t=t; i=i; j=j})
            )
          ret
        elif j = 0 then
          // Edge AB
          let local_elts = 
            if i = 1 then
              if N = 2 then
                [
                getCanonicalEdgeElements ts N (a,c) 1;
                getCanonicalEdgeElements ts N (c,b) (N-1)
                ]
              else
                let norm = getCanonicalEdgeElements ts N (a,c) 1
                [ { t = t; i = i; j = 1}; 
                  norm ]

            elif i = (N-1) then
              let norm = getCanonicalEdgeElements ts N (c,b) (N-1)
              [  norm; 
                { t = t; i = i-1; j = 1} ]
            else
              [ { t = t; i = i-1; j = 1}; 
                { t = t; i = i; j = 1} ]
          let idx = i
          let key = (a,b)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          let ret = non_local @ local_elts
          ret |> List.iter(fun x -> 
            let y = normalizeElement ts x
            if y <> x then 
              printfn "Non-Normalized Neighbour %s of %s" (vtxStr x) (vtxStr {t=t; i=i; j=j})
            )
          ret
        elif (i+j) = N then
          // Edge BC
          
          let local_elts = 
            if i = 1 then              
              if N = 2 then
                [
                getCanonicalEdgeElements ts N (a,c) 1;
                getCanonicalEdgeElements ts N (a,b) 1
                ]
              else//  [1][N-1] is close to 'C'
                let norm = getCanonicalEdgeElements ts N (a,c) (N-1)
                [ norm;
                  { t = t; i = i; j = j-1} ]
            elif j = 1 then //  [N-1][1] is close to 'B'
              let norm = getCanonicalEdgeElements ts N (a,b) (N-1)
              
              [ { t = t; i = i-1; j = j}; 
                norm ]
            else
              [ { t = t; i = i-1; j = j}; 
                { t = t; i = i; j = j-1} ]
          let idx = j
          let key = (b,c)
          let non_local = getOtherEdgeNeighbours ts N t key idx
          let ret = non_local @ local_elts
          ret |> List.iter(fun x -> 
            let y = normalizeElement ts x
            if y <> x then 
              printfn "Non-Normalized Neighbour %s of %s" (vtxStr x) (vtxStr {t=t; i=i; j=j})
            )
          ret
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
          |> List.map(normalizeElement ts)

  let uniformCoordinate (rng : System.Random) (oneDtoTwoD : Map<int,int*int>) (ts : TriangleSet<'A>)  =
    let t = rng.Next(0,20)
    let N = (Array.length ts.triangles.[t].points) - 1
    let ij = rng.Next(0, (N+1)*(N+2)/2)
    let (i,j) = Map.find ij oneDtoTwoD
    { t = t; i = i; j = j }

  
  let getVertexNeighboursFromUrl (ts : TriangleSet<'A>) (url : VertexUrl) =
    let st = ts.triangles.[url.t]
    getVertexNeighbours ts st url.t (url.i, url.j)

  let urlToKey (ts : TriangleSet<KeyedPoint<'A>>) url = 
    let t = ts.triangles.[url.t]
    let p = t.points.[url.i].[url.j]
    p.key

  let keyToUrl (ts : TriangleSet<KeyedPoint<'A>>) (ti : int) (key : (char*int) list) = 
    let t = ts.triangles.[ti]
    let N = (Array.length t.points) - 1

    let (a,b,c) = extractKey t 
    let na = key |> List.filter(fun kv -> fst kv = a) |> List.sumBy snd
    let nb = key |> List.filter(fun kv -> fst kv = b) |> List.sumBy snd
    let nc = key |> List.filter(fun kv -> fst kv = c) |> List.sumBy snd
    let sum = na + nb + nc
    let factor =
      if sum < N then
        N / sum
      else
        1
    { t = ti; i = nb * factor; j = nc * factor}    

  let getAllPointsWithDuplicates (ts : TriangleSet<'A>) =
    ts.triangles
    |> List.ofArray
    |> List.collect(fun t -> t.points |> List.ofArray |> List.collect(List.ofArray))
   
  let getAllPointsOnce (ts : TriangleSet<'A>) =
    ts.triangles
    |> Array.indexed
    |> List.ofArray
    |> List.collect(fun (ti,t) -> 
      let np1 = Array.length t.points
      t.points
      |> Array.indexed
      |> List.ofArray
      |> List.collect(fun (i,arr) ->
        arr 
        |> Array.indexed
        |> List.ofArray
        |> List.filter(fun (j, pt) ->
          let isEdge =
            i = 0 || j = 0 || (i+j) = (np1-1)
          if isEdge then 
            let url = { i = i; j=j; t=ti }
            url = normalizeElement ts url
          else
            true
        )
        |> List.map snd
      ))
    
