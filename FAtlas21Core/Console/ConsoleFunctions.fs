namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes

module ConsoleFunctions =
  let printIcosa printer (id : TriangleSet<KeyedPoint<Coordinate>>) =
    printer <| sprintf "IcosaDivision Scale=%i" id.triangles.[0].points.Length

  let printClusterAssignment printer (cs : ClusterAssigmentState<(char*int) list>) =
    printer <| sprintf "ClusterAssignment #Clusters=%i" cs.allClusters.Length

  let printClusterFinished printer (cca : CompleteClusterAssignment<(char*int) list>) =
    printer <| sprintf "ClusterFinished #Clusters=%i" cca.allClusters.Length

  let printTectonicAssigned printer (tec : TectonicData<(char*int) list>) =
    printer <| sprintf "TectonicAssigned #Plates=%i" tec.plates.Length

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