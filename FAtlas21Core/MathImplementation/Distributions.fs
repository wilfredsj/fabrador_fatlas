namespace FAtlas

open MathTypes
open LinAlgUtils

module Distributions = 

  let correlate_questionable rho u v =
    (u, rho * u + v * sqrt (1.0-rho*rho))

    

  let boxMullerPair (rng : System.Random) () =
    let lower = 1e-20 //Guess
    let mutable u1 = rng.NextDouble()
    while u1 < lower do
      u1 <- rng.NextDouble()
    let u2 = rng.NextDouble()

    let r = u1 |> log |> (*) -2.0 |> sqrt
    let th = 2.0 * System.Math.PI * u2
        
    (r * cos th, r * sin th)

  let normalDistn (rng : System.Random) mean stdev () =
    let (u1, _) = boxMullerPair rng ()
    (mean + u1 * stdev)



    
  let correlatedNormals (rng : System.Random) rho s1 s2 =
    let s1_2 = s1*s1
    let s2_2 = s2 * s2
    let s12_2 = rho * s1 * s2
    // S = (s1_2, s12_2)
    //     (s12_2, s2_2)

    let S = [| [| s1_2; s12_2|]; [| s12_2; s2_2 |]|]

    let S_05 = sqrtMatrix { x = S }

    let sim () =
      let (u1, u2) = boxMullerPair rng ()
      let vs= matrixMultiplyVec S_05.x [| [| u1 |]; [| u2 |] |]
      (vs.[0].[0], vs.[1].[0])

    sim
