namespace FAtlas

open CoordFunctions
open TriangleMeshTypes
open TriangleLookupTypes


module TriangleLookupFunctions =

  let fifthAngle = System.Math.PI * 2.0 / 5.0

  // Cartesian []
  let checkDirections = 

    // Canonically these check directions are ICEG from the below schema
    // Check directions are * *, antipodean directions are _ _

    // TOP -                *I*
    //                                 
    // Upper -     A  *E*  J  *G* _B_ [A]...
    // Lower -      _F_ *C*  D  _H_  K  [F]...
    //
    // BOTTOM               _L_
    let phi = (1.0 + sqrt(5.0)) / 2.0
    let one = 1.0
    [| (phi, 0.0, one); 
      (0.0, one, -phi); 
      (one, phi, 0.0);
      (one, -phi, 0.0) |]
    |> Array.map(fun (x,y,z) -> cart x y z |> normalize)

  // Cartesian -> Cartesian -> double
  let dirSorter n1 n2 c =
    let dn1 = dot n1 c
    let dn2 = dot n2 c
    let angle = System.Math.Atan2(dn2, dn1)
    if angle < 0.0 then
      angle + System.Math.PI * 2.0
    else
      angle

  // Cartesian -> char * Cartesian list -> char * Cartesian * double []
  let dkListToSortedArray normal1 normal2 = 
    List.map(fun (k,c) -> (k, c, dirSorter normal1 normal2 c)) 
    >> List.sortBy(fun (_, _, x) -> x) >> Array.ofList

  // Cartesian list -> Cartesian -> (Cartesian, char * Cartesian * double [] )
  let extractNormalAndSort list dir =
    let upperRef = list |> List.head
    let upperNormal = cross dir (upperRef |> snd) |> normalize
    let n2 = cross upperNormal dir
    (upperNormal, n2, list |> dkListToSortedArray n2 upperNormal)

  // Cartesian * double list -> char -> Cartesian -> HalfPartition
  let makePartitionHalf dir list key top =
    let threshold = list |> List.map(snd) |> List.min
    let (n1, n2, boundary) = extractNormalAndSort (list |> List.map(fst)) top
    let acthr = 
      if dir then
        System.Math.Acos threshold
      else 
        System.Math.Acos (-threshold)
    { upDirection = top; relevantKey = key; boundary = boundary; normal = n2; threshold = threshold; normal2 = n1; acosThr = acthr }

  // (Cartesian, char) -> char -> Cartesian * double list -> Cartesian * double list -> Partition
  let makePartition (top,topDir) bottomKey upper lower =
    { upper = makePartitionHalf true upper top topDir; lower = makePartitionHalf false lower bottomKey topDir}

  let partitionDirection (keys : DirectionKey list) normal = 
    keys 
    |> List.map(fun (k,v) -> ((k,v), dot v normal)) 
    |> List.sortBy(snd)
    |> List.rev
    |> function
      | [] -> failwith "Empty list"
      | top :: top' ->
        // top : ((char, Cartesian), double)
        let (upperRaw, upperRaw') = top' |> List.splitAt(5)
        let (lowerRaw, bottom') = upperRaw' |> List.splitAt(5)
        let bottom = bottom' |> List.head
        makePartition (fst top) (bottom |> fst |> fst) upperRaw lowerRaw

  let makeAllPartitions keys = 
    checkDirections
    |> Array.map(partitionDirection keys)

  let cached (uts : TriangleSet<'a>) = 
    let keys = uts.frame.frameMap |> Map.toList
    { uncached = uts; partitions = makeAllPartitions keys; lookup = Map.empty }

  // Boolean -> HalfPartition -> Cartesian -> PartitionKeyResult Option
  let getKeysFrom dir hp candidate =
    let thisDot = dot (hp.upDirection) candidate
    let isInside = if dir then thisDot >= hp.threshold else thisDot <= hp.threshold
    if isInside then
      let candidate2 = cross candidate hp.upDirection
      let dn1 = dot hp.normal candidate2 |> float
      let dn2 = dot hp.normal2 candidate2 |> float
      let theta' = System.Math.Atan2 (dn2, dn1) - (System.Math.PI / 2.0)
      let theta = if theta' < 0.0 then theta' + 2.0 * System.Math.PI else theta'
      let epochDouble = theta / fifthAngle |> System.Math.Truncate
      let epoch = epochDouble |> int
      let excessTheta = theta - epochDouble
      let nextEpoch = (epoch + 1) % hp.boundary.Length
      let refBoundary = hp.boundary.[epoch] |> deKey
      let nextBoundary = hp.boundary.[nextEpoch] |> deKey
      let key = (hp.relevantKey, refBoundary, nextBoundary)
      let impliedAngle = 
        if dir then
          System.Math.Acos thisDot
        else
          System.Math.Acos (-thisDot)
      let arc = impliedAngle / hp.acosThr
      let pkr = { pkrKey = key; spoke = hp.relevantKey; radius = arc; angle = excessTheta; refDir = refBoundary }
      Some(pkr)
    else
      None

  // Cartesian -> Partition -> PartitionKeyResult Option
  let getKeysFromPartition candidate partition =
    candidate
    |> getKeysFrom true partition.upper 
    |> function
       | None -> getKeysFrom false partition.lower candidate
       | Some(x) -> Some(x) 

  // Partition [] -> Cartesian -> PartitionKeyResult
  let findKey partitions candidate =
    partitions
    |> Array.pick(getKeysFromPartition candidate)

  // CachedTriangleSet -> char*char*char -> SingleTriangle
  let getTriangleForKey cts key = 
    Map.tryFind key cts.lookup
    |> function
      | None ->
        let (a,b,c) = key
        let sortedKeyList = a :: b :: c :: [] |> List.sort
        let ret = 
          cts.uncached.triangles 
          |> Array.find(fun x -> x.keys |> List.sort |> fun l -> l = sortedKeyList)
        cts.lookup <- Map.add key ret cts.lookup
        ret
      | Some(r) -> r
 
  // CachedTriangleSet -> Cartesian -> PartitionResult
  let getLocalTriangle cts coord =
    let pkr = findKey cts.partitions coord
    let triangle = getTriangleForKey cts pkr.pkrKey
    { pkr = pkr; triangle = triangle } 

  let getNearestKeys cts candidate = 
    cts.uncached.frame.frameMap 
    |> Map.toList 
    |> List.map(
      fun (key, coord) -> 
        coord - candidate 
        |> modulusSq 
        |> fun x -> (key, coord, x)) 
    |> List.sortBy(fun (_,_,m) -> m)