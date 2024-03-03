namespace FAtlas

module ConsoleTypes =
  type ConsoleAction =
  | Print
  | Stats
  | Details
  | AHelp
  | Plot

  type ConsoleTarget = 
  | State
  | GeoMesh
  | Tectonics
  | Cluster
  | THelp

  type CachedArg = 
  | LastTriangle of int
  | LastSeaLevel of float Option
  | Dummy

  let isSameKey (arg1:CachedArg) (arg2:CachedArg) =
    match arg1, arg2 with
    | LastTriangle i1, LastTriangle i2 -> true
    | LastSeaLevel f1, LastSeaLevel f2 -> true
    | _ -> false

  let updatedCachedArg (oldList : CachedArg list) (newElt:CachedArg) =
    oldList
    |> List.filter(isSameKey newElt >> not)
    |> List.append [newElt]

  let mergeCachedArgs (old : CachedArg list) (newList: CachedArg list) =
    newList
    |> List.fold(fun oldAcc newElt -> updatedCachedArg oldAcc newElt) old
      
  type ConsoleCell = { text : char; escapeText : string option; postText : string option }

  type ConsoleLine = ConsoleCell array
  type ConsoleGrid = { lines : ConsoleLine array; width : int }

  type ConsoleGridWrite = {  row : int; col : int; str : string; escapeString : string option }
  let gridWritePlain row col str = { row = row; col = col; str = str; escapeString = None }
  let gridWrite row col str escapeString = { row = row; col = col; str = str; escapeString = escapeString }

  let defaultConsoleGrid () = { lines = Array.empty; width = 160 }
  
