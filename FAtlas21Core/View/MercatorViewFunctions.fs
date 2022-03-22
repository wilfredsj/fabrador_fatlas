namespace FAtlas

module MercatorViewFunctions =
  let createSolid pointMaker colourMaker numMeridians numLatitudes =

    // printfn "Create mercator grid %i meridians %i latitudes" numMeridians numLatitudes
    let pi = System.Math.PI
    
    let northPole = (0.0, 0.0)
    let southPole = (pi, 0.0)

    let meridScale = 2.0 * pi / (float numMeridians)
    let latScale = pi / (float (1 + numLatitudes))

    let meridians = 
      [1 .. numMeridians]
      |> List.collect(fun mi ->
        [1 .. numLatitudes]
        |> List.map(fun lj ->
          ((float lj) * latScale, (float (mi - 1)) * meridScale)))

    let allCoordinates = northPole :: southPole :: meridians |> Array.ofList
    let allPoints = allCoordinates |> Array.map pointMaker
    let allColours = allCoordinates |> Array.map colourMaker


    // Triangles:
    // (North Pole, (0,k), (0,k+1) for each k in meridians)
    // (i,k), (i+1, k+1), (i, k+1)
    // (i,k), (i+1, k), (i+1, k+1)
    // (n,k), South Pole, (n, k+1)

    let getIndex mi lj =
      2 + mi * numLatitudes + lj

    


    let northPoleLinks = 
      [0 .. (numMeridians-1)] |> List.collect(fun mi -> [0; getIndex mi 0; getIndex ((mi + 1) % numMeridians) 0])

    let southPoleLinks = 
      [1 .. (numMeridians-1)] |> List.collect(fun mi -> [getIndex mi (numLatitudes-1); 1; getIndex ((mi + 1) % numMeridians) (numLatitudes-1)])

    let normalLinksIJ = 
      [0 .. (numMeridians-1)] 
      |> List.collect (fun mi -> 
        let mi' = (mi + 1) % numMeridians
        [0 .. (numLatitudes - 2)] 
        |> List.collect(fun lj ->
          let lj' = lj + 1
          [(mi, lj); (mi', lj'); (mi', lj)]
          |> List.map (fun (x,y) -> getIndex x y)))

    let normalLinksJI = 
      [0 .. (numMeridians-1)] 
      |> List.collect (fun mi -> 
        let mi' = (mi + 1) % numMeridians
        [0 .. (numLatitudes - 2)] 
        |> List.collect(fun lj ->
          let lj' = lj + 1
          [(mi, lj); (mi, lj'); (mi', lj')]
          |> List.map (fun (x,y) -> getIndex x y)))


    let links = 
      List.concat [northPoleLinks; southPoleLinks; normalLinksIJ; normalLinksJI]
      |> Array.ofList

    //printfn "All points:"
    //allCoordinates |> Array.iteri(fun i (ex,ey)-> printfn "%i    %f %f" i ex ey)
    //printfn "All Triangles:"
    //links |> Array.chunkBySize(3) |> Array.iter(
    //  function 
    //  | [| i1; i2; i3 |] -> printfn "%i %i %i" i1 i2 i3)

    (allPoints, allColours, links, "Triangles")

