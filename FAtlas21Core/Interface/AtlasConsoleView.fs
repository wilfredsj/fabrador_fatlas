namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open AtlasStateTypes
open ConsoleFunctions
open AtlasViewFunctions
open ConsoleTypes

module AtlasConsoleView =

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
    | _ ->
      printer "Stats not supported for this target"
    state, []

  let consoleDetails printer state (cc : ConsoleCommandTyped) = 
    let lastArgs = 
      state.consoleCache.cachedArgs
    let argsUsed = 
      match cc.target with
      | Cluster ->
        match tryExtractCompleteClusterData state.model with
        | Some cs -> clusterDetails printer lastArgs cc.args cs
        | None -> 
          printer "No Cluster"
          []
      | GeoMesh ->
        match tryExtractGeoMesh state.model with
        | Some gds -> geoHeightDetails printer lastArgs cc.args gds
        | None -> 
          printer "No GeoMesh"
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
      | Details -> consoleDetails printer state cc
      
    let args' = mergeCachedArgs state.consoleCache.cachedArgs argsUsed
    { newState with consoleCache = { newState.consoleCache with lastConsoleCommand = Some cc; cachedArgs = args'} }

