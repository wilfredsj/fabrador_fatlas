namespace FAtlas

open AtlasStateTypes
open System.Text.RegularExpressions

module AtlasIO = 

  type PartialMessage = { chars : string }

  type IOMatch =
    | ExactMatch of Message
    | PartialMatch of Message
    | NoMatch

  // Below (comment included) copied from Active Pattern documentation on Microsoft website

  // ParseRegex parses a regular expression and returns a list of the strings that match each group in
  // the regular expression.
  // List.tail is called to eliminate the first element in the list, which is the full matched expression,
  // since only the matches for each group are wanted.
  let (|ParseRegex|_|) regex str =
     let m = Regex(regex).Match(str)
     if m.Success
     then Some (List.tail [ for x in m.Groups -> x.Value ])
     else None

  let partialMessage str =
    match str with
    | ParseRegex "^q(\d+)$" [newSeed] -> ReSeed (System.Int32.Parse newSeed) |> PartialMatch
    | "qq" ->                    Restart                     |> ExactMatch
    | "qw" ->                    Divide 1                    |> ExactMatch
    | "qee" ->                   ClusterInit None            |> ExactMatch
    | "qr" ->                    ClusterIterate 100          |> PartialMatch
    | "qrr" ->                   ClusterIterate 10000        |> ExactMatch
    | "z" -> IcosaView GrayScale            |> NewRenderMode |> ExactMatch
    | "x" -> IcosaView TectonicColours      |> NewRenderMode |> ExactMatch
    | "c" -> { colours = TectonicColours; wireframeConnections = true } 
                             |> ClusterView |> NewRenderMode |> ExactMatch
    | "d" -> { colours = TectonicColours; wireframeConnections = false }
                             |> ClusterView |> NewRenderMode |> ExactMatch
    | "v" ->                   MercatorView |> NewRenderMode |> ExactMatch
    | "aa" -> Rotate_X true  |> ForceRotate |> UIInstruction |> ExactMatch
    | "aw" -> Rotate_Y true  |> ForceRotate |> UIInstruction |> ExactMatch
    | "aq" -> Rotate_Z true  |> ForceRotate |> UIInstruction |> ExactMatch
    | "ad" -> Rotate_X false |> ForceRotate |> UIInstruction |> ExactMatch
    | "as" -> Rotate_Y false |> ForceRotate |> UIInstruction |> ExactMatch
    | "ae" -> Rotate_Z false |> ForceRotate |> UIInstruction |> ExactMatch
    | "az" ->    Rotate_Stop |> ForceRotate |> UIInstruction |> ExactMatch
    | "ax" ->    Rotate_Stop |> ForceRotate |> UIInstruction |> ExactMatch
    | "[" ->                 ForceEuclidian |> UIInstruction |> ExactMatch
    | "]" ->                 ForceMercator  |> UIInstruction |> ExactMatch 
    | _ -> NoMatch

  let forceMessage m =
    match m with
    | ExactMatch m' -> m'
    | PartialMatch m' -> m'
    | NoMatch -> NoOp

  let fullMessage pm =
    ({chars = ""}, forceMessage (partialMessage pm.chars))

  let addToMessage pm ch = 
    let newStr = System.String.Concat [pm.chars; string ch]
    match partialMessage newStr with
    | ExactMatch msg -> 
      ({ chars = "" }, Some msg)
    | _ ->
      ({ chars = newStr}, None)

  