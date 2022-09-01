namespace FAtlas

open CoordTypes

module CoordFunctions =
  let cartFromSphere s =
    let slong = System.Math.Sin s.latitude
    { x = slong * System.Math.Cos s.longitude; y = slong * System.Math.Sin s.longitude; z = System.Math.Cos s.latitude }


  let cart x y z = { x = x; y = y; z = z}

  let origin = cart 0.0 0.0 0.0
  let zrot cart c s =
    { x = cart.x * c + cart.y * s; y = cart.x * (-s) + cart.y * c; z = cart.z }

  let modulusSq c = c.x * c.x  + c.y * c.y + c.z * c.z
  let modulus = modulusSq >> System.Math.Sqrt
  let dot a b = a.x * b.x + a.y * b.y + a.z * b.z
  let cross a b = { x = a.y * b.z - a.z * b.y; y = a.z * b.x - a.x * b.z; z = a.x * b.y - a.y * b.x }
  let angle a b = dot a b / (System.Math.Sqrt((modulusSq a) * (modulusSq b))) |> System.Math.Acos
  let scale c d = { x = c.x * d; y = c.y * d; z = c.z * d }
  let scale' d c = { x = c.x * d; y = c.y * d; z = c.z * d }
  let normalize c = c |> modulusSq |> System.Math.Sqrt |> fun f -> (1.0 / f) |> scale c 
  let overNormalize o c = c |> modulusSq |> fun m -> m / o |> System.Math.Sqrt |> fun f -> (1.0 / f) |> scale c 

  let cartFromSphereWithRadius r = cartFromSphere >> scale' r

  let cartToExternal v3 cart =
    v3 (cart.x |> float32, cart.y |> float32, cart.z |> float32)

  
  let coordFromCart c =
    let r = c.x * c.x + c.y * c.y |> sqrt
    let lat = atan2 r c.z
    let long = atan2 c.y c.x
    { latitude = lat; longitude = long }
    
  let centroid (inputs : Cartesian list) =
    match inputs with
    | [] -> origin
    | h :: tl -> 
      let myFold (c,w) (elt) = ((c+ elt), w + 1.0)
      let init = (h,1.0)
      tl 
      |> List.fold myFold init
      |> fun (agg, mass) -> agg / (float mass)

  let weightedCentroid (inputs : (Cartesian*float) list) =
    match inputs with
    | [] -> origin
    | (h,w) :: tl -> 
      let myFold (c,w) (elt, ew) = (c+ (elt*ew), w + ew)
      let init = (h*w,w)
      tl 
      |> List.fold myFold init
      |> fun (agg, mass) -> agg / (float mass)

  // atan2 returns in [-pi , pi]
  // more reasonably for sorting we want range [0, 2pi]
  let bearing hub dref dref2 (x : Cartesian) =
    let dx = x - hub
    let cos_part = dot dref dx
    let sin_part = dot dref2 dx
    let at = atan2 sin_part cos_part
    if at < -1e-6 then
      at + 2.0 * System.Math.PI
    else
      at

  let prepareBearing (hub : Cartesian) reference =
    let dref = reference - hub
    // assume hub is on unit sphere
    // So outward unit vector same as 'hub'
    let dref2 = cross dref hub
    (normalize dref, normalize dref2)


  let getBearings hub reference op xs =
    let (dref, dref2) = prepareBearing hub reference
    List.map(fun x -> (x, bearing hub dref dref2 <| op x)) xs

  let makeBearinger hub reference =
    let (dref, dref2) = prepareBearing hub reference
    bearing hub dref dref2

  let reconstitute_dx dref dref2 bearing =
    (dref * cos bearing) + (dref2 * sin bearing)

  let getReconstituted hub reference xs =
    let (dref, dref2) = prepareBearing hub reference
    List.map(reconstitute_dx dref dref2) xs

  // These are the 20 vertices of a dodecahedron
  let dodecahedronVertices =
    let phi = (1.0 + sqrt(5.0)) / 2.0;
    let invPhi = 1.0 / phi;
    let dodecahedronPoints = Array.create 20 origin
    dodecahedronPoints.[0] <- cart invPhi phi 0.0 
    dodecahedronPoints.[1] <- cart -invPhi phi 0.0 
    dodecahedronPoints.[2] <- cart 1.0 1.0 1.0 
    dodecahedronPoints.[3] <- cart -1.0 1.0 1.0 
    dodecahedronPoints.[4] <- cart -1.0 1.0 -1.0 
    dodecahedronPoints.[5] <- cart 1.0 1.0 -1.0 
    dodecahedronPoints.[6] <- cart 0.0 invPhi phi 
    dodecahedronPoints.[7] <- cart 0.0 invPhi -phi 
    dodecahedronPoints.[8] <- cart phi 0.0 invPhi 
    dodecahedronPoints.[9] <- cart -phi 0.0 invPhi 
    dodecahedronPoints.[10] <- cart -phi 0.0 -invPhi 
    dodecahedronPoints.[11] <- cart phi 0.0 -invPhi 
    dodecahedronPoints.[12] <- cart 0.0 -invPhi phi 
    dodecahedronPoints.[13] <- cart 0.0 -invPhi -phi 
    dodecahedronPoints.[14] <- cart 1.0 -1.0 1.0 
    dodecahedronPoints.[15] <- cart -1.0 -1.0 1.0 
    dodecahedronPoints.[16] <- cart -1.0 -1.0 -1.0 
    dodecahedronPoints.[17] <- cart 1.0 -1.0 -1.0 
    dodecahedronPoints.[18] <- cart invPhi -phi 0.0 
    dodecahedronPoints.[19] <- cart -invPhi -phi 0.0 
    dodecahedronPoints
    |> Array.map(normalize)

  // These are the 12 groupings of the above 20 vertices
  let dodecahedronFaceIndices =
    let creator = function
      | 0 -> [0u; 2u; 6u; 3u; 1u]
      | 1 -> [0u; 5u; 7u; 4u; 1u]
      | 2 -> [0u; 5u; 11u; 8u; 2u]
      | 3 -> [1u; 3u; 9u; 10u; 4u]
      | 4 -> [7u; 5u; 11u; 17u; 13u]
      | 5 -> [2u; 8u; 14u; 12u; 6u]
      | 6 -> [6u; 12u; 15u; 9u; 3u]
      | 7 -> [10u; 4u; 7u; 13u; 16u]
      | 8 -> [11u; 17u; 18u; 14u; 8u]
      | 9 -> [10u; 16u; 19u; 15u; 9u]
      | 10 -> [13u; 17u; 18u; 19u; 16u]
      | 11 -> [12u; 15u; 19u; 18u; 14u]
      | _ -> failwith "i >= 12"
    Array.init 12 creator

  let toArrayOfTriangles indices =
    let arr = Array.ofList indices
    let n = Array.length arr 
    let funToI i =
      match i % 3 with
      | 0 -> arr.[0]
      | 1 -> arr.[(i / 3) + 1]
      | 2 -> arr.[(i / 3) + 2]
      | _ -> failwith "Unexpected"
        
    Array.init (3 * (n-2)) funToI

  // 12 vertices of an icosahedron
  let icosahedronVertices = 
    let phi = (1.0 + sqrt(5.0)) / 2.0;
    let icosahedronPoints = Array.create 12 origin
    icosahedronPoints.[0] <- cart 0.0 1.0 phi
    icosahedronPoints.[1] <- cart 0.0 -1.0 phi
    icosahedronPoints.[2] <- cart 0.0 1.0 -phi
    icosahedronPoints.[3] <- cart 0.0 -1.0 -phi
    icosahedronPoints.[4] <- cart 1.0 phi 0.0
    icosahedronPoints.[5] <- cart -1.0 phi 0.0
    icosahedronPoints.[6] <- cart 1.0 -phi 0.0
    icosahedronPoints.[7] <- cart -1.0 -phi 0.0
    icosahedronPoints.[8] <- cart phi 0.0 1.0
    icosahedronPoints.[9] <- cart phi 0.0 -1.0
    icosahedronPoints.[10] <- cart -phi 0.0 1.0
    icosahedronPoints.[11] <- cart -phi 0.0 -1.0
    icosahedronPoints
    |> Array.map(normalize)

  // These are the 20 groupings of the above 12 vertices
  let icosahedronFaceIndices =
    let creator = function
      | 0 -> [1u; 0u; 8u]
      | 1 -> [1u; 8u; 6u]
      | 2 -> [1u; 6u; 7u]
      | 3 -> [1u; 7u; 10u]
      | 4 -> [1u; 10u; 0u]

      | 5 -> [0u; 5u; 4u]
      | 6 -> [0u; 4u; 8u]
      | 7 -> [8u; 4u; 9u]
      | 8 -> [8u; 9u; 6u]
      | 9 -> [6u; 9u; 3u]
      | 10 -> [6u; 3u; 7u]
      | 11 -> [7u; 3u; 11u]
      | 12 -> [7u; 11u; 10u]
      | 13 -> [10u; 11u; 5u]
      | 14 -> [10u; 5u; 0u]

      | 15 -> [2u; 5u; 4u]
      | 16 -> [2u; 4u; 9u]
      | 17 -> [2u; 9u; 3u]
      | 18 -> [2u; 3u; 11u]
      | 19 -> [2u; 11u; 5u]
      | _ -> failwith "i >= 20"
    Array.init 20 creator