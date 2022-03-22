namespace FAtlas

open TriangleMeshTypes
open TriangleLookupTypes

module TriangleInterpolationFunctions =
  let sixthAngle = System.Math.PI * 2.0 / 6.0

  // suppose y = Sum w_i x_i + w_3 x_3
  // suppose x_3 = x_2 + x_1 - x_0
  // then w': w_0 - w_3
  //          w_1 + w_3
  //          w_2 + w_3

  // int list -> int list
  let makeThreeWeight ws = 
    match ws with
    | [w0;w1;w2;w3] ->  [w0 - w3; w1 + w3; w2 + w3]
    | _ -> ws

  // double -> int -> (int, double)
  // x is scaled [0,1)
  let bucketAndRemainder (x : double) (N : int) =
    let xN = x * (double N) |> float
    let fbucket = xN |> System.Math.Truncate
    let rem = xN - (fbucket)
    (int fbucket, rem)

  // SingleTriangle -> double -> double -> (TrianglePoint list * double list)
  // radius :
  //       radius = 0 <=> at A
  //       radius = 1 <=> along BC line
  // arc :
  //       arc = 0    <=> along AB line
  //       arc = pi/3 <=> along AC line
  let getBoundingPoints st radius arc =
    let p i j = st.points.[i].[j]
    if radius <= 0.0 then
      // A
      let a = p 0 0
      ([a],[1.0])
    else if arc >= 1.0 && radius <= 1.0 then 
      // B
      let pt = p 0 st.scale
      ([pt],[1.0])
    else if arc >= 1.0 && radius >= 1.0 then 
      // C
      let pt = p st.scale 0
      ([pt],[1.0])
    else if radius >= 1.3 then
      failwithf "Radius passed in >1.3 = radius=%.3f, arc=%.3f" radius arc
    else if radius >= 1.0 then
      // BC
      let Np1 = st.scale + 1
      let modArc = arc / System.Math.PI * 3.0 
      let arcNumBuckets = Np1
      let (arcBucket, arcAlpha) = bucketAndRemainder modArc arcNumBuckets
      let i = st.scale - arcBucket
      let j = arcBucket
      let points = [p i j; p (i-1) (j+1)]
      let weights = [1.0 - arcAlpha; arcAlpha]
      (points, weights)
    else if arc <= 0.0 then 
      // AB
      let Np1 = st.scale + 1
      let (radiusBucket, radiusAlpha) = bucketAndRemainder radius st.scale
      let points = [p radiusBucket 0; p (radiusBucket+1) 0]
      let weights = [1.0 - radiusAlpha; radiusAlpha]
      (points, weights)
    else if arc >= 1.0 then
      // AC
      let Np1 = st.scale + 1
      let (radiusBucket, radiusAlpha) = bucketAndRemainder radius st.scale
      let points = [p 0 radiusBucket; p 0 (radiusBucket+1)]
      let weights = [1.0 - radiusAlpha; radiusAlpha]
      (points, weights)
    else
      let Np1 = st.scale + 1
      let (radiusBucket, radiusAlpha) = bucketAndRemainder radius st.scale
      let modArc = arc / System.Math.PI * 3.0 
      let arcNumBuckets = radiusBucket
      let (arcBucket, arcAlpha) = bucketAndRemainder modArc arcNumBuckets
      let i = radiusBucket - arcBucket
      let j = arcBucket
      let UL = p i j
      let UR = p i (j+1)
      let DL = p (i+1) j
      let (a, a') = (1.0 - radiusAlpha, radiusAlpha)
      let (b, b') = (1.0 - arcAlpha, arcAlpha)
      let fourWgts = [a * b; a' * b; a * b'; a' * b'] 
      if (radiusBucket + 1) >= st.scale then
        // 3-point formula
        let threeWgts = makeThreeWeight fourWgts
        let points = [UL; DL; UR]
        (points, threeWgts)
      else 
        // 4-point
        let DR = p (i+1) (j+1)
        let points = [UL; DL; UR; DR]
        (points, fourWgts)

  let flipRef b pkr =
    { pkr with refDir = b; angle = sixthAngle - pkr.angle } 

  let flipSpoke pkr =
    let perp = (System.Math.Sin pkr.angle) * pkr.radius
    let par = (System.Math.Cos pkr.angle) * pkr.radius
    let par' = 1.0 - par
    let angle' = System.Math.Atan2 (perp, par')
    let modSq = (perp * perp) + (par' * par')
    let radius' = System.Math.Sqrt modSq
    { pkr with refDir = pkr.spoke; spoke = pkr.refDir; radius = radius' ; angle = angle' }

  let rotatePKR pkr a b c =
    if pkr.spoke = a then
      if pkr.refDir = b then
        pkr
      else
        flipRef b pkr
    else if pkr.spoke = b then
      if pkr.refDir = a then
        flipSpoke pkr
      else  
        // Flip s.t. refDir = a, other = c
        // Re-Spoke s.t. refDir = b, spoke = a, other = c
        pkr |> flipRef a |> flipSpoke
    else
      // Spoke = c
      if pkr.refDir = a then
        pkr |> flipSpoke |> flipRef b
      else
        // Flip s.t. refDir = a, other = b
        // Re-Spoke s.t. refDir = c, spoke = a, other = b
        // Flip s.t. refDir = b, spoke = a, other = c
        pkr |> flipRef a |> flipSpoke |> flipRef b
      
  let rotatePR pr a b c = { pr with pkr = rotatePKR pr.pkr a b c }

  let evaluateBounds pr =
    getBoundingPoints pr.triangle pr.pkr.radius pr.pkr.angle 

  //double list -> TrianglePoint list -> (TrianglePoint -> double) -> double
  let interpolateTriangle weights points f = 
    weights |> List.zip(points) |> List.sumBy(fun (p, w) -> w * (f p))

  //let getInterpolatedTemperature cts coord =
  //  let partitionResult = Cache.getLocalTriangle cts coord
  //  let (a,b,c) = partitionResult.pkr.pkrKey
  //  let rotatedPR = rotatePR partitionResult a b c
  //  let (points, weights) = evaluateBounds rotatedPR
  //  interpolateTriangle weights points interpolateHeat 


