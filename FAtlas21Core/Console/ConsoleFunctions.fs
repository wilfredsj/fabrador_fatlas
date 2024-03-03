namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open TriangleMeshFunctions
open UtilTypes
open ConsoleTypes
open ConsoleGridFunctions
open GeoMeshFunctions
open CoordFunctions

module ConsoleFunctions =

  type LegendRow<'A> = {
    value : 'A; 
    actualText : string;
    unescapedText : string
  }

  let keyValuesForLegend =
    [0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0]

  let getLegendKeyValues (escaper : float -> string) (charer : float -> string) keyValues =
    keyValues 
    |> List.map(fun v -> 
      let actualText = sprintf "%s| %-3f : %s%s |" resetAnsiEscapeColour v (String.concat "" [| escaper v; charer v |]) resetAnsiEscapeColour
      let unescapedText = sprintf "| %-3f : %s |" v (String.concat "" [| charer v |])
      { value = v; actualText = actualText; unescapedText = unescapedText })

  let getLegendKeyValuesGrid (grid: ConsoleGrid) (escaper : float -> string) (charer : float -> string) (keyValues : float list) =
    let writes =
      keyValues 
      |> List.indexed
      |> List.collect(fun (row,v) -> 
        let asString3 = sprintf "%.3f" v
        let escape = escaper v
        let char = charer v
        [
          gridWritePlain row 0 "|";
          gridWritePlain row 2 asString3;
          gridWritePlain row 8 ":";
          gridWrite row 10 (string char) (Some escape);
          gridWritePlain row 12 "|"
        ]
      )
    writeStringsIntoGrid grid writes

  let intToAsciiChar (i : int) = 
    // single character for values up to 60
    if i < 10 then
      sprintf "%i" i
    elif i < 36 then
      sprintf "%c" (char (i + 55))
    elif i < 62 then
      sprintf "%c" (char (i + 61))
    else
      "?"

  let intOptToAsciiChar (iOpt : int option) = 
    match iOpt with
    | Some i -> intToAsciiChar i
    | None -> "?"

  let intOptRedIfMissing (iOpt : int option) = 
    match iOpt with
    | Some i -> ""
    | None -> "\x1b[31m"

  let floatToAsciiChar (f : float) =
    // heavier character for higher values
    let chars = [|'.'; ':'; '-'; '='; '+'; '*'; '#'; '%'; '@'|]
    let nChars = chars.Length
    let f' = int <| f * float nChars
    if f' < 0 then
      chars.[0] |> string
    elif f' >= nChars then
      chars.[nChars - 1] |> string
    else
      chars.[f'] |> string

  let writeTriangleToGrid grid (f : 'A -> 'B) (mainPrinter : 'B -> string) (fancyPrinterOpt : ('B -> string) option) (triangle : SingleTriangle<'A>) =
    let np1 = triangle.points.[0].Length
    let writes =
      [0..np1-1]
      |> List.collect(fun row ->
        [0..row]
        |> List.map(fun col ->
          let i = row-col
          let j = col
          let v = f triangle.points.[i].[j]

          let writeRow = row
          let writeCol = np1 - row + 2 * col

          match fancyPrinterOpt with
          | Some fancyPrinter -> 
            gridWrite writeRow writeCol (mainPrinter v) (Some <| fancyPrinter v) 
          | None ->
            gridWritePlain writeRow writeCol (mainPrinter v)
          )
       )
    writeStringsIntoGrid grid writes

  let printTriangleGrid grid printer (f : 'A -> 'B) (mainPrinter : 'B -> string) (fancyPrinterOpt : ('B -> string) option) (triangle : SingleTriangle<'A>) =
    let grid' = writeTriangleToGrid grid f mainPrinter fancyPrinterOpt triangle
    printGrid grid' printer

  let writeTriangleToGrid_Int grid (f : 'A -> int) (triangle : SingleTriangle<'A>) =
    writeTriangleToGrid grid f intToAsciiChar None triangle
    
  let writeTriangleToGrid_Float grid (f : 'A -> float) (triangle : SingleTriangle<'A>) =
    writeTriangleToGrid grid f floatToAsciiChar (Some floatToAnsiEscapeColour) triangle

  let printIcosa printer (id : TriangleSet<KeyedPoint<Coordinate>>) =
    printer <| sprintf "IcosaDivision Scale=%i" id.triangles.[0].points.Length

  let printClusterAssignment printer (cs : ClusterAssigmentState<(char*int) list>) =
    printer <| sprintf "ClusterAssignment #Clusters=%i" cs.allClusters.Length

  let printClusterFinished printer (cca : CompleteClusterAssignment<(char*int) list>) =
    printer <| sprintf "ClusterFinished #Clusters=%i" cca.allClusters.Length

  let printTectonicAssigned printer (tec : TectonicData<(char*int) list>) =
    printer <| sprintf "TectonicAssigned #Plates=%i" tec.plates.Length

  let getSeaLevelArg (lastArgs : CachedArg list) args =
    args 
    |> List.tryPick(fun s -> 
      match s with
      | ParseRegex "s=([0-9.]+)" [s] -> Some(Some(float s))
      | ParseRegex "s=[yY]" [] -> Some(Some(1.0))
      | ParseRegex "s=[nN]" [] -> Some(None)
      | ParseRegex "s=" [] -> Some(Some(1.0))
      | _ -> None)
    |> Option.defaultValue(
      lastArgs 
      |> List.tryPick(fun s -> 
        match s with
        | LastSeaLevel fOpt -> fOpt
        | _ -> None))

  let getClusterIdArg (lastArgs : CachedArg list) args =
    args 
    |> List.tryPick(fun s -> 
      match s with
      | ParseRegex "c=([0-9]+)" [c] -> 
        let c' = int c
        Some(c')
      // case for single letter indicating 10-19
      | ParseRegex "c=([\w])" [cx] ->
        let c = cx.ToLower()
        let offset = 'a'
        let c' = (int c.[0] - int offset) + 10
        Some(c')
      | _ -> None)
    |> Option.defaultValue 0

  let getTriangleArg (lastArgs : CachedArg list) args =
    args 
    |> List.tryPick(fun s -> 
      match s with
      | ParseRegex "t=([pPnN])" [a] ->
        let t = a.ToLower().[0]
        let offset = 
          if t = 'p' then
            19
          else
            1
        let last =
          lastArgs 
          |> List.tryPick(fun s ->
            match s with
            | LastTriangle i -> Some i
            | _ -> None)
          |> Option.defaultValue 0
        Some((last + offset) % 20)
      | ParseRegex "t=([0-9]+)" [t] -> 
        let t' = int t
        // It's an icosahedron so:
        if t' < 20 then
          Some(t')
        else
          None
      // case for single letter indicating 10-19
      | ParseRegex "t=([aAbBcCdDeEfFgGhHiIjJ])" [tx] ->
        let t = tx.ToLower()
        let offset = 'a'
        let t' = (int t.[0] - int offset) + 10
        Some(t')
      | _ -> None)
    |> Option.defaultValue 0

  let getNonCanonicalArg args =
    args
    |> List.tryPick(fun s ->
      match s with
      | ParseRegex "nc=([tTfF]*)$" [nc] -> 
        let nc' = nc.ToLower()
        if nc' = "t" then
          Some(true)
        elif nc' = "f" then
          Some(false)
        elif nc' = "" then
          Some(true)
        else
          None
      | _ -> None)
    |> Option.defaultValue false

  let plotClusterDetails printer lastArgs args (cs : CompleteClusterAssignment<'A>) =
    let t = getTriangleArg lastArgs args
    let nc = getNonCanonicalArg args
    let ts =  cs.meshData
    let triangle = ts.triangles.[t]
    let grid = defaultConsoleGrid ()
    let clusterIdFrom ts' (b : BasicPoint) = 
      let key = b.key 
      let url = keyToUrl ts' t key
      cs.clusterAssignments
      |> Map.tryFind url
      |> Option.defaultWith(fun () -> 
        let url2 = normalizeElement ts url
        cs.clusterAssignments
        |> Map.tryFind url2
        |> Option.defaultWith(fun () -> 
          failwith <| sprintf "Could not find cluster for %A" url))
    let clusterIdFrom_CanonicalOnly ts' (b : BasicPoint) = 
      let key = b.key 
      let url = keyToUrl ts' t key
      let url2 = normalizeElement ts url
      if url = url2 then
        cs.clusterAssignments
        |> Map.tryFind url
        |> Option.orElseWith(fun () -> 
          failwith <| sprintf "Could not find cluster for %A" url)
      else
        None

    let grid' =
      if nc then
        writeTriangleToGrid grid (clusterIdFrom_CanonicalOnly ts) intOptToAsciiChar (Some intOptRedIfMissing) triangle
      else 
        writeTriangleToGrid_Int grid (clusterIdFrom ts) triangle

    printGrid grid' printer
    [LastTriangle t]

  let addTriNeighboursToGrid grid ts t i j =
    let nbrMap = getAllOtherTriangles ts t
    //   * - - * - - *
    //    \ a / \ c /
    //     \ / t \ /
    //      * - - *
    //       \ b /
    //        \ /
    //         *
    let a = nbrMap.[OE_BA]
    let b = nbrMap.[OE_CB]
    let c = nbrMap.[OE_AC]
    let writes =
      [
        gridWritePlain i j "* - - * - - *";
        gridWritePlain (i+1) j (sprintf " \\ %s / \\ %s / " (intToAsciiChar a) (intToAsciiChar c));
        gridWritePlain (i+2) j (sprintf "  \\ / %s \\ /  " (intToAsciiChar t));
        gridWritePlain (i+3) j "   * - - *   ";
        gridWritePlain (i+4) j (sprintf "    \\ %s /    " (intToAsciiChar b));
        gridWritePlain (i+5) j "     \\ /     ";
        gridWritePlain (i+6) j "      *      "
        ]
    writeStringsIntoGrid grid writes

  let geoHeightDetails printer lastArgs args gds =
    let t = getTriangleArg lastArgs args
    let seaLevelOpt = getSeaLevelArg lastArgs args
    let scale = getGeoScale (defaultGeoConstants) gds.triangleSet.triangles.[t]
    printer <| sprintf "t=%i" t
    printer <| sprintf "scale (m) = %.1f" scale
    [LastTriangle t; LastSeaLevel seaLevelOpt] 
    

  let plotGeoHeightDetails printer lastArgs args gds =
    let t = getTriangleArg lastArgs args
    let seaLevelOpt = getSeaLevelArg lastArgs args
    let N = gds.triangleSet.triangles.[t].points.Length
    let grid = defaultConsoleGrid ()
    let grid' = 
      match seaLevelOpt with
      | None ->         
          writeTriangleToGrid_Float grid (fun p -> 4.0*(p.datum.r-1.0)) gds.triangleSet.triangles.[t]
      | Some seaLevel ->
          let f = (fun p -> 
            if p.datum.r > seaLevel then
              4.0*(p.datum.r-1.0), true
            else
              0.0, false) 
          let asciiPrinter (f, isLand) = 
            if isLand then
              floatToAsciiChar f
            else
              "~"
          let fancyPrinter (f, isLand) = 
            if isLand then
              floatToAnsiEscapeColour f
            else
              //BLUE escape code:
              "\x1b[34m"
          writeTriangleToGrid grid f asciiPrinter (Some fancyPrinter) gds.triangleSet.triangles.[t]
    let grid'' = 
      getLegendKeyValuesGrid grid' floatToAnsiEscapeColour floatToAsciiChar keyValuesForLegend

    let grid''' =
      addTriNeighboursToGrid grid'' gds.triangleSet t 0 ((2 * N) - 12)

    printGrid grid''' printer
    [LastTriangle t; LastSeaLevel seaLevelOpt]

  let printGeoDivision printer (gds : GeoDivisionState<(char*int) list>) =
     printer <| sprintf "GeoDivision Scale=%i" gds.triangleSet.triangles.[0].points.Length

  let getMeanAndStDev values = 
    let mean = values |> Array.average
    let meanSq = values |> Array.map (fun v -> v * v) |> Array.average
    let stDev = sqrt (meanSq - mean * mean)
    mean, stDev

  let printHistogramFloat printer (nBins : int) (values : float array) =
    let min = values |> Array.min
    let max = values |> Array.max
    let range = max - min
    let overStep = 0.05 * range
    let minUsed = min - overStep
    let maxUsed = max + overStep
    let actRange = maxUsed - minUsed
    let binSize = actRange / float nBins
    let binIds = values |> Array.map (fun v -> int ((v - minUsed) / binSize))
    let binCounts = Array.init nBins (fun i -> binIds |> Array.filter (fun id -> id = i) |> Array.length)
    let maxBinCount = binCounts |> Array.max
    let countString count =       
      let countF = float count
      let countFrac = countF / (float maxBinCount)
      let nChars = 
        if maxBinCount > 20 then
          countFrac * 20.0 |> int
        else
          count
      String.replicate nChars "#"
    let printBin i binCount =
      let binMin = minUsed + float i * binSize
      let binMax = binMin + binSize
      let pad1 = if binMin < 0.0 then "" else " "
      let pad2 = if binMax < 0.0 then "" else " "
      printer <| sprintf "[%s%.4f, %s%.4f): %-20s" pad1 binMin pad2 binMax (countString binCount)

    printer <| sprintf "Histogram nBins=%i" nBins
    binCounts |> Array.iteri printBin
    printer <| sprintf "Histogram End"

  let printHistogramDiscrete printer (values : 'a array) = 
    let valueCounts = 
      values 
      |> Array.groupBy id 
      |> Array.map (fun (v, vs) -> v, vs.Length)
      |> Array.sortBy (fun (v, count) -> -count)
    let maxCount = valueCounts |> Array.map snd |> Array.max
    let countString count =       
      let countF = float count
      let countFrac = countF / (float maxCount)
      let nChars = 
        if maxCount > 20 then
          countFrac * 20.0 |> int
        else
          count
      String.replicate nChars "#"
    let printBin (v, count) =
      printer <| sprintf "%-20s: %-20s" (sprintf "%A" v) (countString count)

    printer <| sprintf "Histogram"
    valueCounts |> Array.iter printBin
    printer <| sprintf "Histogram End"
   

  let tectonicStats printer (tec : TectonicData<(char*int) list>) =
    let allHeightBiases = tec.plates |> Array.map (fun p -> p.heightBias)
    // print height bias distribution
    let (mean, stdDev) = getMeanAndStDev allHeightBiases
    printer <| sprintf "HeightBias mean=%.4f stdDev=%.4f, n=%i" mean stdDev allHeightBiases.Length
    printHistogramFloat printer 10 allHeightBiases

    let allBorders = 
      tec.plates 
      |> Array.collect (fun p -> 
        p.stressNeighboursSorted 
        |> Array.filter(fun (a,b) -> a.thisId < a.oppositeId))

    let allStresses = 
      allBorders
      |> Array.map (fun (a,b) -> a.stress)

    let (mean, stdDev) = getMeanAndStDev allStresses
    printer <| sprintf "Stress mean=%.4f stdDev=%.4f n=%i" mean stdDev allStresses.Length
    printHistogramFloat printer 15 allStresses

    let allForms = 
      allBorders
      |> Array.map (fun (a,b) -> a.form)

    printHistogramDiscrete printer allForms

  let tectonicDetails printer lastArgs args (tec : TectonicData<(char*int) list>) =
    let c = getClusterIdArg lastArgs args
    let thisPlate = tec.plates.[c-1]
    let thisCluster = thisPlate.cluster
    let (dref, dref2) = prepareBearing thisCluster.orderedBorder.hub thisCluster.orderedBorder.ref
    let velocityBearing = bearing thisCluster.orderedBorder.hub dref dref2 thisPlate.unitVelocity
    printer <| sprintf "Plate ID %i" c
    printer <| sprintf "HeightBias %.2f" thisPlate.heightBias
    printer <| sprintf "Velocity %.3f, bearing %.3f" thisPlate.velocityMagnitude velocityBearing
    printer <| "    NBId   | NB_HB  | Stress  | Bearing | Form"
    thisPlate.stressNeighboursSorted |> Array.iter (fun (a,b) -> printer <| sprintf "    %-2i (%s) | %6.3f |  %6.3f | %5.3f   | %A" a.oppositeId (intToAsciiChar a.oppositeId) a.oppositeMidBias a.stress a.thisBearing a.form)
    []


  let clusterStats printer (cca : CompleteClusterAssignment<(char*int) list>) =
    let allClusterSizes = cca.allClusters |> Array.map (fun c -> c.members |> List.length |> float)
    let (mean, stdDev) = getMeanAndStDev allClusterSizes
    printer <| sprintf "ClusterSize mean=%.4f stdDev=%.4f, n=%i" mean stdDev allClusterSizes.Length
    printHistogramFloat printer 10 allClusterSizes

  let geoHeightStats printer (ts : TriangleSet<KeyedPoint<GeoMeshDatum>>) = 
    let allPoints = getAllPointsOnce ts
    let allPointsBad = getAllPointsWithDuplicates ts
    let allHeights = allPoints |> List.map(fun p -> p.datum.r) |> Array.ofList
    let (mean, stdDev) = getMeanAndStDev allHeights
    printer <| sprintf "Num vertices=%i (before de-duplication=%i)" allPoints.Length allPointsBad.Length
    printer <| sprintf "Height mean=%.4f stdDev=%.4f, n=%i" mean stdDev allHeights.Length
    printHistogramFloat printer 20 allHeights