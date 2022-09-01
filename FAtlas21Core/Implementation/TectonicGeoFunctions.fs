namespace FAtlas


open TectonicTypes
open CoordFunctions

module TectonicGeoFunctions =

  let computeStress (c1,v1,h2) (c2,v2,h2) =
    // Stress >0 => peak
    // Stress <0 => trough
    -dot v1 v2

  let getVelocityForCluster (c : CompleteClusterDatum) v b =
    let [v3] = getReconstituted c.orderedBorder.hub c.orderedBorder.ref [b]
    v3 * v

  let addGeo rng (clusters : CompleteClusterAssignment<_>) = 
    let heightRng (rng : System.Random) () =
      let u = rng.NextDouble()
      (2.0 * u - 1.0)
    let velocityRng (rng : System.Random) () =
      let u = rng.NextDouble()
      u
    let bearingRng (rng : System.Random) () =
      let u = rng.NextDouble()
      (2.0 * System.Math.PI * u)
    let hvbArray = 
      clusters.allClusters 
      |> Array.mapi(fun i _ -> (i, (heightRng rng (), velocityRng rng (), bearingRng rng ())))
    let stressPairs = 
      clusters.connectedFaces
      |> Set.map(fun (i,j) -> 
        let ci = clusters.allClusters.[i]
        let (hi,vi,bi) = hvbArray.[i]
        let cj = clusters.allClusters.[j]
        let (hj,vj,bj) = hvbArray.[j]
        let v3i = getVelocityForCluster ci vi bi
        let v3j = getVelocityForCluster cj vj bj
        let stress_ij = computeStress (ci,v3i,hi) (cj,v3j, hj)
        ((i,j), stress_ij)
      |> Set.toArray
    let sortedStress = 
      stressPairs 
      |> Array.copy
      |> Array.sortBy
    1

