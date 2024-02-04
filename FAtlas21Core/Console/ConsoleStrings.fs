namespace FAtlas

open ConsoleTypes
module ConsoleStrings =

  let consoleActionMap = 
      [ "print", Print
        "info", Print
        "stats", Stats
        "details", Details
        "help", AHelp ]
      |> Map.ofList

  let parseConsoleAction str = consoleActionMap |> Map.tryFind str

  let keysWithUniqueValues map =
    map
    |> Map.toSeq
    |> Seq.distinctBy snd
    |> Seq.map fst

  let uniqueReversedMap map =
    map
    |> Map.toSeq
    |> Seq.distinctBy snd
    |> Seq.map (fun (k, v) -> v, k)
    |> Map.ofSeq
    

  let uniqueActions = keysWithUniqueValues consoleActionMap

  let consoleTargetMap = 
    [ "state", State
      "tectonics", Tectonics
      "tec", Tectonics
      "tect", Tectonics
      "geo", GeoMesh
      "geomesh", GeoMesh
      "help", THelp
      "cluster", Cluster
      "clu", Cluster]
    |> Map.ofList

  let consoleTargetToString target = 
    let lookup = uniqueReversedMap consoleTargetMap 
    lookup
    |> Map.find target

  let parseConsoleTarget str = consoleTargetMap |> Map.tryFind str
  let uniqueTargets = keysWithUniqueValues consoleTargetMap