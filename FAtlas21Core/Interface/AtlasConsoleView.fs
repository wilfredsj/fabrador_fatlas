namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open AtlasStateTypes
open ConsoleFunctions
open AtlasViewFunctions
open ConsoleTypes
open ConsoleStrings

module AtlasConsoleView =

  let actionTargetCombinations = Map.ofList [
    Print, [State; Tectonics; GeoMesh]
    Stats, [State; Tectonics; GeoMesh; Cluster]
    Plot, [Cluster; GeoMesh]
    Details, [Tectonics; GeoMesh]
    ] 

  let printTargetsFor printer action =
    printer "Available targets:"
    actionTargetCombinations |> Map.find action |> List.iter (fun t -> printer (t |> consoleTargetToString))

  let printStateSummary state printer =
    let stateModel = state.model
    match stateModel with
    | Init -> printer "Init"
    | IcosaDivision id -> printIcosa printer id
    | ClusterAssignment (cs, _) -> printClusterAssignment printer cs
    | ClusterFinished cca -> printClusterFinished printer cca
    | TectonicAssigned tec -> printTectonicAssigned printer tec
    | GeoDivision gds -> printGeoDivision printer gds

  let consolePrint printer state (cc : ConsoleCommandTyped) =
    match cc.target with
    | State ->
      printStateSummary state printer
    | GeoMesh ->
      match tryExtractGeoMesh state.model with
      | Some gds -> printGeoDivision printer gds
      | None -> printer "No GeoMesh"
    | Tectonics -> 
      match tryExtractTectonicData state.model with
      | Some tec -> printTectonicAssigned printer tec
      | None -> printer "No Tectonics"
    | THelp -> printTargetsFor printer Print
    | _ ->
      printer "Print not supported for this target"
      
    state, []

  let consoleStats printer state (cc : ConsoleCommandTyped) =
    match cc.target with
    | State ->
      match tryExtractTectonicData state.model with
      | Some tec -> tectonicStats printer tec
      | None -> printer "No Tectonics"
    | Tectonics -> 
      match tryExtractTectonicData state.model with
      | Some tec -> tectonicStats printer tec
      | None -> printer "No Tectonics"
    | Cluster ->
      match tryExtractCompleteClusterData state.model with
      | Some cs -> clusterStats printer cs
      | None -> printer "No Cluster"
    | GeoMesh ->
      match tryExtractGeoMesh state.model with
      | Some gds -> geoHeightStats printer gds.triangleSet
      | None -> printer "No GeoMesh"
    | THelp -> printTargetsFor printer Stats
    | _ ->
      printer "Stats not supported for this target"
    state, []

  let consolePlot printer state (cc : ConsoleCommandTyped) = 
    let lastArgs = 
      state.consoleCache.cachedArgs
    let argsUsed = 
      match cc.target with
      | Cluster ->
        match tryExtractCompleteClusterData state.model with
        | Some cs -> plotClusterDetails printer lastArgs cc.args cs
        | None -> 
          printer "No Cluster"
          []
      | GeoMesh ->
        match tryExtractGeoMesh state.model with
        | Some gds -> plotGeoHeightDetails printer lastArgs cc.args gds
        | None -> 
          printer "No GeoMesh"
          []
      | THelp -> 
        printTargetsFor printer Plot
        []
      | _ ->
        printer "Details not supported for this target"
        []
    state, argsUsed
    
  let consoleHelp printer state (cc : ConsoleCommandTyped) =
    printer "Available Commands:"
    uniqueActions |> Seq.iter (fun a -> printer a)
    state, []

  let consoleDetails printer state cc =
    let lastArgs = 
      state.consoleCache.cachedArgs
    let argsUsed = 
      match cc.target with
      | GeoMesh ->
        match tryExtractGeoMesh state.model with
        | Some gds -> geoHeightDetails printer lastArgs cc.args gds
        | None -> 
          printer "No GeoMesh"
          []
      | Tectonics -> 
        match tryExtractTectonicData state.model with
        | Some tec -> tectonicDetails printer lastArgs cc.args tec
        | None -> 
          printer "No Tectonics"
          []
      | THelp -> 
        printTargetsFor printer Details
        []
      | _ ->
        printer "Details not supported for this target"
        []
    state, argsUsed

  let processConsoleCommand printer state (cc : ConsoleCommandTyped) = 
    let (newState, argsUsed) = 
      match cc.action with
      | Print -> consolePrint printer state cc
      | Stats -> consoleStats printer state cc
      | Plot -> consolePlot printer state cc
      | AHelp -> consoleHelp printer state cc
      | Details -> consoleDetails printer state cc
      
    let args' = mergeCachedArgs state.consoleCache.cachedArgs argsUsed
    { newState with consoleCache = { newState.consoleCache with lastConsoleCommand = Some cc; cachedArgs = args'} }

