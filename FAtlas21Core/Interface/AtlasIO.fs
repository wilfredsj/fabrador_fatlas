namespace FAtlas

open AtlasStateTypes

type PartialMessage = { chars : string }

module AtlasIO =
  let partialMessage str =
    match str with
    | "qq" -> Restart |> Some
    | "qw" -> Divide 1 |> Some
    | "qee" -> ClusterInit None |> Some
    | "qre" -> ClusterIterate 100 |> Some
    | "qrr" -> ClusterIterate 10000 |> Some
    | "z" -> IcosaView GrayScale       |> NewRenderMode |> Some
    | "x" -> IcosaView TectonicColours |> NewRenderMode |> Some
    | "c" -> ClusterView { colours = TectonicColours; wireframeConnections = true } |> NewRenderMode |> Some
    | "d" -> ClusterView { colours = TectonicColours; wireframeConnections = false }|> NewRenderMode |> Some
    | "v" -> Some <| NewRenderMode MercatorView
    | _ -> None

  let fullMessage pm =
    ({chars = ""}, Option.defaultValue NoOp (partialMessage pm.chars))

  let addToMessage pm ch = 
    let newStr = System.String.Concat [pm.chars; string ch]
    match partialMessage newStr with
    | Some msg -> 
      ({ chars = "" }, Some msg)
    | None ->
      ({ chars = newStr}, None)

  