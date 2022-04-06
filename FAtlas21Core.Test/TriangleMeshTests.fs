module FAtlas21Core.TriangleMeshTests

open NUnit.Framework

open FAtlas.Interface
open FAtlas.AtlasStateTypes
open FAtlas.AtlasIO
open FAtlas.TectonicFunctions
open FAtlas.TriangleMeshTypes
open FAtlas.TriangleMeshFunctions

let emptyCallback = { makeVertex = (fun x -> 1); makeColour = (fun x -> 1); onUpdateCallback = fun x -> () }



let createSetupIcosahedron n =    
  let data = createIcosahedron()
  let divided = 
    [1 .. n] 
    |> List.fold (fun t i -> divideIcosahedron t) data
  divided

let ts4 = createSetupIcosahedron 4

let isNormalized ts url =
  let url' = normalizeElement ts url
  url' = url

let assertNormalized ts comment url =
  let url' = normalizeElement ts url
  let message = 
    if url' <> url then
      sprintf "Vertex %s not normalized (should be %s). Context: %s" (vtxStr url) (vtxStr url') (comment())
    else
      ""
  Assert.True((url = url'), message)

let verifyNeighboursNormalized ts url =
  let nbs = getVertexNeighboursFromUrl ts url
  List.iteri(fun i nb -> 
    assertNormalized ts (fun () -> sprintf "%i neighbour of %s" i (vtxStr url)) nb) nbs

    
[<Test>]
let TestNonHubCase () =
  let problemCase = { t = 14; i = 13; j = 3}
  verifyNeighboursNormalized ts4 problemCase


// AETHER-22
[<Test>]
let TestHubAdjacent_n4() =
  let problemCase = { t= 15; i = 0 ; j = 15}
  let N = 16
  
  // Is this a contradition Contradiction:
  // Requesting neighbours of CE (15th element)
  // the 4th one, i=15;j=1 is canonically on a different triangle?
  // Maybe this is fine
  
  // The results are canonically on 4 different triangles
  //          "I"
  //           |
  //           |
  //           |   7 
  //           |"E"
  //"A" ------ X X ------  "J"
  //          X o X 
  //     5   / X X    16
  //        /     \
  //       /  15   \
  //      /         \
  //    "F" --------"C"

  // CFE
  let (k15a,k15b,k15c) = extractKey ts4.triangles.[15]
  let hub = getCanonicalVertex ts4 N k15c

  // CEJ
  let (k16a,k16b,k16c) = extractKey ts4.triangles.[16]
  //let hub2 = getCanonicalVertex ts4 N k16b

  Assert.AreEqual(k15c, k16b)
  Assert.AreEqual(k15a, k16a)

  let tgt1 = getIthVertexAlongEdgeAsUrl ts4 N (16, OE_BC) 1
  let n2 = normalizeElement ts4 tgt1

  Assert.AreEqual(n2.t, 7)
  let (k7a,k7b,k7c) = extractKey ts4.triangles.[7]

  let (k5a,k5b,k5c) = extractKey ts4.triangles.[5]
  
  Assert.AreEqual(k5c, k7b)

  let tgt2 = getIthVertexAlongEdgeAsUrl ts4 N (7, OE_BC) 1
  
  Assert.AreEqual(n2, tgt2, "Normalized form of vertex to be from Triangle 7")

  let nbs111 = getVertexNeighboursFromUrl ts4 problemCase

  // Vertex (15, 1)[16] not normalized (should be (15, 1)[7]). Context: 3 neighbour of (0, 15)[15]
 
  verifyNeighboursNormalized ts4 problemCase

[<Test>]
let TestNormalizedVertex () =
  let ts = createSetupIcosahedron 4

  let problemCase = { t = 14; i = 13; j = 3}
  let p2 = { t= 15; i = 0 ; j = 15}
  // It's not normalized...
  let n' = isNormalized ts problemCase

  let nbs = getVertexNeighboursFromUrl ts problemCase

  let nbsN = List.map(fun n -> 
    let n' = normalizeElement ts n
    (n, n', n = n')) nbs

  let e1 = getOtherEdgeNeighbours ts 16 15 ('C', 'E') 15

  

  let e2 = List.map(normalizeElement ts) e1

  
  //assertNormalized ts vtxStr problemCase
  ()


