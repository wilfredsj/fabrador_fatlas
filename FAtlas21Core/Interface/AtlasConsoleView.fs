namespace FAtlas

open TriangleMeshTypes
open CoordTypes
open TectonicTypes
open GeoMeshTypes
open AtlasStateTypes
open ConsoleFunctions
open AtlasViewFunctions

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
    state

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
    | _ ->
      printer "Stats not supported for this target"
    state
      
  let processConsoleCommand printer state (cc : ConsoleCommandTyped) = 
    match cc.action with
    | Print -> consolePrint printer state cc
    | Stats -> consoleStats printer state cc

