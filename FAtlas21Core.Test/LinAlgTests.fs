module FAtlas21Core.LinAlgTests

open NUnit.Framework

open FAtlas.MathTypes
open FAtlas.LinAlgUtils


[<Test>]
let TestDeterminant () =
  let dumbMatrixOne = { 
    x = [|
      [| 4.0; 1.0 |];
      [| 2.0; 2.0 |]
    |]
  }
  Assert.AreEqual(6.0, det2 dumbMatrixOne, 0.001)

let assertEqualm2 caseNum (expected : Matrix2x2) (actual : Matrix2x2) tolerance =
  (Array.zip expected.x actual.x)
  |> Array.iteri(fun i (exis, acis) ->
    (Array.zip exis acis)
    |> Array.iteri(fun j (exij, acij) ->
      Assert.AreEqual(exij, acij, tolerance, sprintf "Case %i: Matrices not same at %i %i %f > %f" caseNum i j (exij - acij) tolerance)
      )
    )

let verifyEigenVector caseNum matrix evec eval tolerance =
  let actual = matrixMultiplyVec matrix (vectorAsMatrix evec) |> extractColumn 0
  let expected = evec |> Array.map(fun vi -> vi * eval)
  Array.iteri2 (fun i exi aci ->
    Assert.AreEqual(exi, aci, tolerance, sprintf "Case %i: Vectors not same at %i %f > %f" caseNum i (exi - aci) tolerance)
  ) expected actual 

[<Test>]
let TestInverse () = 
  let i = eye [| 1.0; 1.0 |] |> fun xs -> { x = xs }
  let samples = [
      [|
         [| 4.0; 1.0 |];
         [| 2.0; 2.0 |]
      |] ;
      [|
         [| -1.0; 0.0 |];
         [| 0.0; 2.0 |]
      |] ; 
      [|
         [| 1.0; 0.0 |];
         [| 0.0; -2.0 |]
      |]
  ] 
  samples
  |> List.map(fun y -> { x = y })
  |> List.iteri(
    fun caseNum m -> 
      let mi = invert2 m
      let i2 = m2 mi m 
      assertEqualm2 caseNum i i2 0.000001
  )

[<Test>]
let TestSDV () =
  let samples = [
      [|
         [| 4.0; 1.0 |];
         [| 2.0; 2.0 |]
      |] ;
      [|
         [| -1.0; 0.0 |];
         [| 0.0; 2.0 |]
      |] ; 
      [|
         [| 1.0; 0.0 |];
         [| 0.0; -2.0 |]
      |]
  ] 
  samples
  |> List.map(fun y -> { x = y })
  |> List.iteri(
    fun caseNum m ->
      let svdi = luv2 m
      let mtm = m2 (t2 m) m
      verifyEigenVector caseNum mtm.x (evFromSVD svdi 0) (svdi.evs.[0]*svdi.evs.[0]) 0.00001
      verifyEigenVector caseNum mtm.x (evFromSVD svdi 1) (svdi.evs.[1]*svdi.evs.[1]) 0.00001
      let m' = pow 1.0 svdi 
      let d2 = det2 m'
      let d1 = det2 m
      let d3_1 = det2 svdi.u
      let d3_2 = det2 (t2 svdi.v)
      assertEqualm2 caseNum m m' 0.000001
      )


[<Test>]
let testSDV_Sqrt () = 
  // Only symmetric positive definite here!
  let samples = [
      [|
         [| 4.0; 2.0 |];
         [| 2.0; 3.0 |]
      |] ;
      [|
         [| 1.0; 0.0 |];
         [| 0.0; 2.0 |]
      |] ; 
      [|
         [| 1.0; 0.2 |];
         [| 0.2; 2.0 |]
      |]
  ] 
  samples
  |> List.map(fun y -> { x = y })
  |> List.iteri(
    fun caseNum m ->
      let svdi = luv2 m
      let sqrt2 = pow 0.5 svdi 
      let m' = m2 sqrt2 sqrt2
      let m'' = m2 (t2 sqrt2) (t2 sqrt2)
      let d2 = det2 m'
      let d15 = det2 sqrt2
      let d1 = det2 m
      assertEqualm2 caseNum m m' 0.000001)



