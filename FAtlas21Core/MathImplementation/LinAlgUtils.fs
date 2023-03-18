namespace FAtlas

open MathTypes 

type RealQuadratic =
| TwoRoots of float*float
| SingleRoot of float
| NoRealRoots

module LinAlgUtils = 

  let eye (vec : float [])= 
    [|
      [| vec.[0]; 0.0|];
      [| 0.0; vec.[1] |]
    |]

  let vectorAsMatrix (v : float[]) =
    Array.map(fun vi -> [| vi |]) v

  let extractColumn i (m : float[][]) =
    m
    |> Array.map(fun is -> is.[i])

  let matrixMultiplyVec (av : float [][]) (bv : float [][]) =
    if Array.isEmpty av then
      [||]
    else 
      let n = Array.length av.[0]
      av |> Array.mapi(fun i ai0 ->
        bv.[0] |> Array.mapi(fun j bj ->
          [| 0 .. (n-1) |]
          |> Array.map(fun k -> av.[i].[k] * bv.[k].[j])
          |> Array.sum))
        

  let m2 (a : Matrix2x2) (b : Matrix2x2) = 
    {
      x = matrixMultiplyVec a.x b.x
    }  

  let tr2 (m : Matrix2x2) =
    m.x.[0].[0] + m.x.[1].[1]

  let t2 (m : Matrix2x2) =
    let x' = 
      [|
        [| m.x.[0].[0] ; m.x.[1].[0] |];
        [| m.x.[0].[1] ;  m.x.[1].[1]|]
      |]
    { x = x' }
    
    
  let pow d suv = 
    let ev' = suv.evs |> Array.map(fun ev -> System.Math.Pow(ev, d))
    let D = { x = eye ev' }
    let vt = t2 suv.v
    m2 suv.u (m2 D vt)

  let evFromSVD (suv : SVD_2x2) i =
    suv.v.x.[i]

  let det2 (m : Matrix2x2) = 
    m.x.[0].[0] * m.x.[1].[1] - m.x.[1].[0] * m.x.[0].[1]

  // Hopefully only used in tests
  let invert2 (m : Matrix2x2) = 
    let d = det2 m
    let x' = 
      [|
        [| m.x.[1].[1] / d; -m.x.[0].[1] / d |];
        [|-m.x.[1].[0] / d;  m.x.[0].[0] / d |]
      |]
    { x = x' }
      
  let normv2 (x : double []) =
    let n = x.[0] * x.[0] + x.[1] * x.[1] |> System.Math.Sqrt
    [| x.[0] / n; x.[1] / n|]

  let realQuadratic a b c =
    let b' = b / a
    let c' = c/a
    let det = b' * b' - 4.0 * c'
    if det > 0.0 then
      let u = sqrt det
      TwoRoots ((-b' + u) / 2.0, (-b'- u) / 2.0)
    elif det = 0.0 then
      SingleRoot (-b' / 2.0)
    else
      NoRealRoots

  let extractEigenVectorForEV (m : Matrix2x2) l = 
    let X = m.x
    if X.[0].[1] <> 0.0 then
      Some [| X.[0].[1]; l - X.[0].[0] |]
    else
      if X.[1].[0] <> 0.0 then
        Some [| l - X.[1].[1]; X.[1].[0] |]
      else
        if abs (X.[0].[0] - l) < 1e-10 then
          Some [| 1.0; 0.0|]
        elif abs (X.[1].[1] - l) < 1e-10 then
          Some [| 0.0; 1.0|]
        else
          None
        
  let luv2_ (m : Matrix2x2) = 
    // Eigenvalues are solution to det (l1 - X) = 0
    // (l-X00)*(l-X11)-X01*X10 = 0
    // l^2 - (X00+X11).l + (X00*X11- X01*X10) = 0
    let eigenvalues = realQuadratic 1.0 (-1.0 * tr2 m) (det2 m)
    match eigenvalues with
    | TwoRoots(l1, l2) ->
      let evs = [| l1; l2|]
      let evecs = 
        evs
        |> Array.map(extractEigenVectorForEV m)
        |> Array.map(Option.get)
        |> Array.map(normv2)
      let D_inv = evs |> Array.map(fun ev -> 1.0 / ev) |> eye |> fun xs -> { x = xs }
      let V = { x = evecs }
      let vt = t2 V
      let U = m2 m (m2 V D_inv)
      { u = U; evs = evs; v = V}

  let luv2 (m : Matrix2x2) = 
    // Eigenvalues are solution to det (l1 - X) = 0
    // (l-X00)*(l-X11)-X01*X10 = 0
    // l^2 - (X00+X11).l + (X00*X11- X01*X10) = 0
    let mtm = m2 (t2 m)m
    let eigenvalues = realQuadratic 1.0 (-1.0 * tr2 mtm) (det2 mtm)
    match eigenvalues with
    | TwoRoots(l1, l2) ->
      let evSQs = [| l1; l2|]
      let evs = [| sqrt l1; sqrt l2|]
      let evecs = 
        evSQs
        |> Array.map(extractEigenVectorForEV mtm)
        |> Array.map(Option.get)
        |> Array.map(normv2)
      let D_inv = evs |> Array.map(fun ev -> 1.0 / ev) |> eye |> fun xs -> { x = xs }
      let V = { x = evecs }
      let vt = t2 V
      let U = m2 m (m2 V D_inv)
      { u = U; evs = evs; v = V}
  

  let sqrtMatrix (m : Matrix2x2) =
    luv2 m
    |> pow 0.5
    




