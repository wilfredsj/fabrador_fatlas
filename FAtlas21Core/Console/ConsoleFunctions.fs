namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open TriangleMeshFunctions
open UtilTypes
open ConsoleTypes

module ConsoleFunctions =

  type LegendRow<'A> = {
    value : 'A; 
    actualText : string;
    unescapedText : string
  }

  let resetAnsiEscapeColour =
    "\x1b[0m"

  let keyValuesForLegend =
    [0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0]

  let getLegendKeyValues (escaper : float -> string) (charer : float -> string) keyValues =
    keyValues 
    |> List.map(fun v -> 
      let actualText = sprintf "%s| %-3f : %s%s |" resetAnsiEscapeColour v (String.concat "" [| escaper v; charer v |]) resetAnsiEscapeColour
      let unescapedText = sprintf "| %-3f : %s |" v (String.concat "" [| charer v |])
      { value = v; actualText = actualText; unescapedText = unescapedText })

  let floatToAnsiEscapeColour (f' : float) =
    //assume f between 0 and 1
    let f = min 1.0 <| max 0.0 f'
    let r = 5 - int (f * 5.0)
    let g = int (f * 5.0)
    let b = 0
    let colour = 16 + b + 6 * g + 36 * r
    sprintf "\x1b[38;5;%im" colour

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

      
  let printTriangle printer (f : 'A -> 'B) (mainPrinter : 'B -> string) (fancyPrinterOpt : ('B -> string) option) (legend : LegendRow<'B> array) (triangle : SingleTriangle<'A>) =
    let np1 = triangle.points.[0].Length
    let extraPadding =
      if legend |> Array.isEmpty then
        0
      else      
        let lastLegendLength = legend.[legend.Length - 1].unescapedText.Length
        if lastLegendLength > (np1 - legend.Length) then
          lastLegendLength - (np1 - legend.Length)
        else
          0
    [0..np1-1]
    |> List.iter(fun row ->
      let mainStr = 
        [|0..row|]
        |> Array.collect(fun col ->
          let i = row-col
          let j = col
          let v = f triangle.points.[i].[j]
          match fancyPrinterOpt with
          | Some fancyPrinter -> 
            [| fancyPrinter v; mainPrinter v; resetAnsiEscapeColour; " " |]
          | None ->
            [| mainPrinter v; " " |]
        )
      let thisLegend = 
        if row < legend.Length then
          Some legend.[row]
        else
          None

      let thisLegendLength = thisLegend |> Option.map(fun x -> x.unescapedText.Length) |> Option.defaultValue 0
      let thisLegendText = thisLegend |> Option.map(fun x -> x.actualText) |> Option.defaultValue ""
      let paddingLength = extraPadding + np1 - row - 1 - thisLegendLength
      let frontPadding = 
        if paddingLength > 0 then
          String.replicate paddingLength " "
        else ""
      let backPadding = String.replicate (np1 - row - 1) " "
      printer <| sprintf "%s%s%s%s" thisLegendText frontPadding (System.String.Concat mainStr) backPadding)
    printer resetAnsiEscapeColour

  let printTriangleInt printer (f : 'A -> int) (triangle : SingleTriangle<'A>) =
    let legend = [||]
    printTriangle printer f intToAsciiChar None legend triangle

  let printTriangleFloat printer (f : 'A -> float) (triangle : SingleTriangle<'A>) =
    let legend = 
      getLegendKeyValues floatToAnsiEscapeColour floatToAsciiChar keyValuesForLegend
      |> Array.ofList
    printTriangle printer f floatToAsciiChar (Some floatToAnsiEscapeColour) legend triangle

  let printIcosa printer (id : TriangleSet<KeyedPoint<Coordinate>>) =
    printer <| sprintf "IcosaDivision Scale=%i" id.triangles.[0].points.Length

  let printClusterAssignment printer (cs : ClusterAssigmentState<(char*int) list>) =
    printer <| sprintf "ClusterAssignment #Clusters=%i" cs.allClusters.Length

  let printClusterFinished printer (cca : CompleteClusterAssignment<(char*int) list>) =
    printer <| sprintf "ClusterFinished #Clusters=%i" cca.allClusters.Length

  let printTectonicAssigned printer (tec : TectonicData<(char*int) list>) =
    printer <| sprintf "TectonicAssigned #Plates=%i" tec.plates.Length

  let getTriangleArg (lastArgs : CachedArg list) args =
    printfn "Debug getTriangleArg lastArgs=%A args=%A" lastArgs args
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

  let clusterDetails printer lastArgs args (cs : CompleteClusterAssignment<'A>) =
    let t = getTriangleArg lastArgs args
    let nc = getNonCanonicalArg args
    let ts =  cs.meshData
    let triangle = ts.triangles.[t]
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

    if nc then
      printTriangle printer (clusterIdFrom_CanonicalOnly ts) intOptToAsciiChar (Some intOptRedIfMissing) Array.empty triangle
    else 
      printTriangleInt printer (clusterIdFrom ts) triangle
    [LastTriangle t]

  let geoHeightDetails printer lastArgs args gds =
    let t = getTriangleArg lastArgs args
    printTriangleFloat printer (fun p -> 4.0*(p.datum.r-1.0)) gds.triangleSet.triangles.[t]
    [LastTriangle t]

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