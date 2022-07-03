namespace FAtlas

open CoordTypes
open ColourTypes
open TriangleMeshTypes
open TectonicTypes
open CoordFunctions
open TriangleMeshFunctions

open TriangleMeshToRender
open TectonicViewFunctions


module ViewHelperFunctions =

  let getNp1 t =
    t.points.Length
    
  let getNp1TS ts = 
    getNp1 ts.triangles.[0]
    
    
  let uniformHue max mkColour i ij k = rangeToFullSat 0.0 max (float i) |> makeRGB|> mkColour
  let uniformHue2 sat value max mkColour i ij k = rangeWithSatValue sat value 0.0 max (float i) |> makeRGB|> mkColour
  let grayscale mkColour i ij k = ((i * 79) % 71) |> float32 |> fun y -> (y / 71.0f) * 0.4f + 0.1f |> fun z -> (z,z,z) |> mkColour
  let allGray mkColour i ij k = 0.7f |> fun z -> (z,z,z) |> mkColour
  
  // Each input set of indices is index *within its own slice*
  // So the indices will be different in the aggregated output for i>1)
  let concat3 strGlType (tuples : ('a[]*'b[]*int [])[]) =
    let (c1,c2,c3) = tuples.[0]
    let n1 = tuples |> Array.fold(fun acc (a,_,_) -> Array.length(a) + acc) 0
    let n2 = tuples |> Array.fold(fun acc (_,_,a) -> Array.length(a) + acc) 0
    let a = Array.create n1 c1.[0]
    let b = Array.create n1 c2.[0]
    let c = Array.create n2 c3.[0]
    tuples 
    |> Array.fold(fun (n1a, n2a) (al,bl,cl) -> 
      al |> Array.indexed |> Array.iter(fun (i,ai) -> a.[n1a+i] <- ai)
      bl |> Array.indexed |> Array.iter(fun (i,bi) -> b.[n1a+i] <- bi)
      cl |> Array.indexed |> Array.iter(fun (i,ci) -> c.[n2a+i] <- ci + n1a)
      (n1a + Array.length al, n2a + Array.length cl)) (0,0)
    |> ignore
    (a,b,c,strGlType)
    
  let tectonicColours (clusterState : ClusterDataForRendering<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.numClusters |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.membership
    |> function 
        | None -> grayscale mkColour i ij k
        | Some(c) -> uniformHue nc mkColour c ij k

  let tectonicColoursFiltered j (clusterState : ClusterDataForRendering<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.numClusters |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.membership
    |> function 
        | Some(c) when c=j -> uniformHue nc mkColour c ij k
        | _ -> grayscale mkColour i ij k
    
  let tectonicColours2 (clusterState : CompleteClusterAssignment<'A>) mkColour i ij k =
    let url = { t = i; i = fst ij; j = snd ij}
    let key = urlToKey clusterState.meshData url
    let nc = clusterState.allClusters |> Array.length |> fun x -> float (x - 1) 
    Map.tryFind key clusterState.lookupFromKey
    |> function 
      | None -> grayscale mkColour i ij k
      | Some(c) -> uniformHue2 0.6f 0.6f nc mkColour c ij k
    
