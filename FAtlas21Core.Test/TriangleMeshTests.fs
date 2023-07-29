module FAtlas21Core.TriangleMeshTests

open NUnit.Framework

open FAtlas.Interface
open FAtlas.AtlasStateTypes
open FAtlas.AtlasIO
open FAtlas.TectonicFunctions
open FAtlas.TriangleMeshTypes
open FAtlas.TriangleMeshFunctions
open FAtlas.CoordFunctions

let createSetupIcosahedron n =    
  let data = createIcosahedron()
  let divided = 
    [1 .. n] 
    |> List.fold (fun t i -> divideIcosahedron t) data
  divided
  
let ts5 = createSetupIcosahedron 5
let ts4 = createSetupIcosahedron 4
let ts3 = createSetupIcosahedron 3
let ts2 = createSetupIcosahedron 2
let ts1 = createSetupIcosahedron 1

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
  
  // Is this a contradiction?
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


// AETHER-23
[<Test>]
let TestHubAdjacent_n1_case1() =
  // Before:
  // 16 / 16 / 16 / 37 / 72
  // After:  
  // 19 / 37 / 72 / 37 / 37

  verifyNeighboursNormalized ts1 { t= 4; i = 1 ; j = 0}


// AETHER-23
[<Test>]
let TestHubAdjacent_n1_case2() =
  // Before:
  // 19 / 37 / 72 / 37 / 37
  // 0  / 37 / 72 / 37 / 37
  
  verifyNeighboursNormalized ts1 { t= 4; i = 1 ; j = 1}

// AETHER-23
[<Test>]
let TestHubAdjacent_n2_case1() =
  // Before:
  // 0  / 37 / 72 / 37 / 37
  // 0  / 23 / 58 / 23 / 23  
  verifyNeighboursNormalized ts2 { t= 4; i = 1 ; j = 0}

// AETHER-23
[<Test>]
let TestHubAdjacent_n2_case2() =
  // Before:
  // 0  / 23 / 58 / 23 / 23  
  // 0  / 0 / 35 / 0 / 0  
  verifyNeighboursNormalized ts2 { t= 5; i = 1 ; j = 0}

// AETHER-23
[<Test>]
let TestHubAdjacent_n3_case1() =
  // Before:
  // 0  / 0 / 35 / 0 / 0  
  // 0  / 0 / 0  / 0 / 0
  verifyNeighboursNormalized ts3 { t= 0; i = 0 ; j = 5}

let allUrlsForSet ts =
  ts.triangles
    |> Array.indexed
    |> List.ofArray
    |> List.collect(fun (ti, t) ->
      let N = Array.length t.points
      [0 .. (N-1)]
      |> List.collect(fun i -> Array.init (N - i) (fun j -> {t = ti; i = i ; j=j}) |> List.ofArray))
    |> fun urls -> (ts,urls)

let testAllUrls (ts,urlList) = 
  let (goodUrls, badUrls) = 
    urlList
    |> List.map(fun u -> 
      let nus = 
        getVertexNeighboursFromUrl ts u 
        |> List.mapi(fun i u' -> 
           let u'' = normalizeElement ts u'
           (i, u', u''))
      let (good, bad) = nus |> List.partition(fun (_,a,b) -> a=b)
      (u, good, bad))
    |> List.partition(fun (url, good, bad) ->
      if List.isEmpty bad then
        true
      else
        false)
  let badUrlsAsString =
    badUrls
    |> List.map(fun (url, good, bad) ->
      let subMessage =
        bad 
        |> List.map(fun (i,nu,_) -> sprintf "%i: %s, " i (vtxStr nu))
        |> System.String.Concat
      sprintf "%s has badly normalized neighbours: %s\n" (vtxStr url) subMessage)
    |> System.String.Concat
  let total = urlList |> List.length
  let badMessage () = sprintf "There are %i/%i vertices with badly normalized neighbours:\n%s" (List.length badUrls) total badUrlsAsString
      
  Assert.True(List.isEmpty badUrls, badMessage())
  
[<Test>]
let TestAllNeighboursNormalizedN5() = 
  ts5 |> allUrlsForSet |> testAllUrls

[<Test>]
let TestAllNeighboursNormalizedN4() = 
  ts4 |> allUrlsForSet |> testAllUrls

[<Test>]
let TestAllNeighboursNormalizedN3() = 
  ts3 |> allUrlsForSet |> testAllUrls

[<Test>]
let TestAllNeighboursNormalizedN2() = 
  ts2 |> allUrlsForSet |> testAllUrls

[<Test>]
let TestAllNeighboursNormalizedN1() = 
  ts1 |> allUrlsForSet |> testAllUrls
  
let testAllUrlsReasonablyAdjacent (ts, urlList) =
  let toCart = urlToCartesian ts 1.0
  let sampleArcRadius =
    match urlList with
    | [] -> failwith "Bad test case"
    | h :: _ ->
        let ns = getVertexNeighboursFromUrl ts h 
        match ns with
        | [] -> failwith "No neighhours"
        | x :: _ ->
          let c1 = toCart h
          let c2 = toCart x
          angle c1 c2
  let tolerance = 1.2
  // Seems wierd it has to be so large
  let lower = sampleArcRadius / tolerance
  let upper = sampleArcRadius * tolerance
  let (goodUrls, badUrls) = 
    urlList
    |> List.map(fun u -> 
      let c1 = toCart u
      let nus = 
        getVertexNeighboursFromUrl ts u 
        |> List.mapi(fun i u' -> 
          let c_i = toCart u'
          (i, u', angle c1 c_i))
      let (good, bad) = nus |> List.partition(fun (_,_,th) -> (th > lower) && (th < upper))
      (u, good, bad))
    |> List.partition(fun (url, good, bad) ->
      if List.isEmpty bad then
        true
      else
        false)
  let badUrlsAsString =
    badUrls
    |> List.map(fun (url, good, bad) ->
      let subMessage =
        bad 
        |> List.map(fun (i,nu,th) -> sprintf "%i: %s,@%f" i (vtxStr nu) th)
        |> System.String.Concat
      sprintf "%s has irregularly spaced neighbours: %s\n" (vtxStr url) subMessage)
    |> System.String.Concat
  let total = urlList |> List.length
  let badMessage () = sprintf "There are %i/%i vertices with irregularly spaced neighbours, bounds=(%f,%f):\n%s" (List.length badUrls) total lower upper badUrlsAsString
  Assert.True(List.isEmpty badUrls, badMessage())


// AETHER-24
[<Test>]
let TestAllNeighboursRegularN4() = 
  ts4 |> allUrlsForSet |> testAllUrlsReasonablyAdjacent

// AETHER-72
[<Test>]
let TestKeyToUrl () =
  let ts = ts4
  let testRoundTrip url =
    let key = urlToKey ts url
    let url' = keyToUrl ts url.t key
    Assert.AreEqual(url, url')
  let urls = 
    [
      {t = 0; i = 0; j = 0};
      {t = 0; i = 0; j = 1};
      {t = 0; i = 0; j = 5};
      {t = 0; i = 1; j = 4};
      {t = 0; i = 3; j = 2}
    ]
  urls
  |> List.iter(fun u -> testRoundTrip u)

