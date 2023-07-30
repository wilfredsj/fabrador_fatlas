namespace FAtlas

module ConsoleTypes =

  type CachedArg = 
  | LastTriangle of int
  | Dummy

  let isSameKey (arg1:CachedArg) (arg2:CachedArg) =
    match arg1, arg2 with
    | LastTriangle i1, LastTriangle i2 -> true
    | _ -> false

  let updatedCachedArg (oldList : CachedArg list) (newElt:CachedArg) =
    oldList
    |> List.filter(isSameKey newElt >> not)
    |> List.append [newElt]

  let mergeCachedArgs (old : CachedArg list) (newList: CachedArg list) =
    newList
    |> List.fold(fun oldAcc newElt -> updatedCachedArg oldAcc newElt) old
      

