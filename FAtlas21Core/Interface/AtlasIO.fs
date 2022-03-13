namespace FAtlas

open AtlasStateTypes

type PartialMessage = { chars : string }

module AtlasIO =
  let partialMessage str =
    match str with
    | "q" -> Some <| NewRenderMode BasicCoordinate
    | "d" -> Some <| Divide 1
    | _ -> None

  let fullMessage str =
    Option.defaultValue NoOp (partialMessage str)

  let addToMessage pm ch = 
    let newStr = System.String.Concat [pm.chars; string ch]
    match partialMessage newStr with
    | Some msg -> 
      ({ chars = "" }, Some msg)
    | None ->
      ({ chars = newStr}, None)

  