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


  let parseConsoleAction str = 
    match str with
    | "print" | "info" -> Some Print
    | "stats" -> Some Stats
    | _ -> None

  let parseConsoleTarget str = 
   match str with
    | "state" -> Some State
    | "tectonics" | "tec" | "tect" -> Some Tectonics
    | "geo" | "geomesh" -> Some GeoMesh
    | "cluster" | "clu" -> Some Cluster
    | _ -> None

  let parseConsoleCommand (str : string) =
    // Split on spaces
    let split = 
      str.Split([|' '|]) 
      |> Array.toList 
      |> List.filter (fun x -> x <> "")
      |> List.map (fun x -> x.ToLower())
    let parsedCommand = 
      match split with
      | [] -> None
      | [action] -> parseConsoleAction action |> Option.map (fun a -> consoleCommand a None [])
      | [action; target] -> 
        match parseConsoleAction action, parseConsoleTarget target with
        | Some action, tOpt -> Some (consoleCommand action tOpt [])
        | _ -> None
      | action::targetStr::args -> 
        match parseConsoleAction action, parseConsoleTarget targetStr with
        | Some action, Some target -> Some (consoleCommand action (Some target) args)
        | Some action, None -> Some (consoleCommand action None (targetStr :: args))
        | _ -> None
    match parsedCommand with
    | Some cmd -> ConsoleCommand cmd
    | None -> UnknownCommand str

  let partialMessage str =
    match str with
    | ParseRegex "^q(\d+)$" [newSeed] -> ReSeed (System.Int32.Parse newSeed) |> PartialMatch
    | "qq" ->                    Restart                     |> ExactMatch
    | "qw" ->                    Divide 1                    |> ExactMatch
    | "qee" ->                   ClusterInit None            |> ExactMatch
    | "qr" ->                    ClusterIterate 100          |> PartialMatch
    | "qrr" ->                   ClusterIterate 10000        |> ExactMatch
    | "qtt" ->                    AssignTectonics             |> ExactMatch
    | "qty" ->                    InitGeoMesh false           |> ExactMatch
    | "qyy" ->                    InitGeoMesh true            |> ExactMatch
    | "z" ->  IcosaView GrayScale                    |> NewRenderMode |> ExactMatch
    | "xx" -> IcosaView (TectonicColours None)       |> NewRenderMode |> ExactMatch
    | "sx" -> GeoMeshView (TectonicColours None, None, false, false)   |> NewRenderMode |> ExactMatch
    | ParseRegex "^x(\d+)x$" [clusterId] -> 
          IcosaView (TectonicColours (Some (System.Int32.Parse clusterId)))  
                                                    |> NewRenderMode |> ExactMatch
    | ParseRegex "^x(\d+)$" [clusterId] -> 
      IcosaView (TectonicColours (Some (System.Int32.Parse clusterId)))  
                                                  |> NewRenderMode |> PartialMatch
    | ParseRegex "^x(\d+)a(\d+)x$" [clusterId; faceId] -> 
      IcosaViewFiltered ((TectonicColours (Some (System.Int32.Parse clusterId))), System.Int32.Parse faceId)
                                                |> NewRenderMode |> ExactMatch
    | ParseRegex "^xa(\d+)x$" [faceId] -> 
      IcosaViewFiltered (TectonicColours None, System.Int32.Parse faceId)
                                                |> NewRenderMode |> ExactMatch
    | "xc" -> IcosaView (TectonicLocalCoordColours None)  |> NewRenderMode |> ExactMatch
    | ParseRegex "^x(\d+)c$" [clusterId] -> 
          IcosaView (TectonicLocalCoordColours (Some (System.Int32.Parse clusterId)))  
                                                     |> NewRenderMode |> ExactMatch
    | ParseRegex "^xa(\d+)c$" [faceId] -> 
      IcosaViewFiltered (TectonicLocalCoordColours None, System.Int32.Parse faceId)
                                                |> NewRenderMode |> ExactMatch
    | "xv" -> IcosaView (TectonicStressColours None)  |> NewRenderMode |> ExactMatch
    | "sv" -> GeoMeshView (TectonicStressColours None, None, false, false)  |> NewRenderMode |> ExactMatch
    | "xb1" -> IcosaView (TectonicHeightBiasColours (None, HB_Flat, HB_None))  |> NewRenderMode |> ExactMatch
    | "xb2" -> IcosaView (TectonicHeightBiasColours (None, HB_None, HB_Flat))  |> NewRenderMode |> ExactMatch
    | "xb3" -> IcosaView (TectonicHeightBiasColours (None, HB_Flat, HB_Flat))  |> NewRenderMode |> ExactMatch
    | "xb01" -> IcosaView (TectonicHeightBiasColours (None, HB_None, HB_Linear))  |> NewRenderMode |> ExactMatch
    | "xb02" -> IcosaView (TectonicHeightBiasColours (None, HB_None, HB_Stressed))  |> NewRenderMode |> ExactMatch
    | "xb4" -> IcosaView (TectonicHeightBias None)  |> NewRenderMode |> ExactMatch
    | "sb4" -> GeoMeshView (TectonicHeightBias None, None, false, false)  |> NewRenderMode |> ExactMatch
    | "sn1" -> GeoMeshView (HeightBestEffort true, None, false, false)  |> NewRenderMode |> ExactMatch
    | "sn2" -> GeoMeshView (HeightBestEffort true, None, true, false)  |> NewRenderMode |> ExactMatch
    | "sn3" -> GeoMeshView (HeightBestEffort true, None, false, true)  |> NewRenderMode |> ExactMatch
    | "sn4" -> GeoMeshView (HeightBestEffort true, None, true, true)  |> NewRenderMode |> ExactMatch
    | ParseRegex "^snn(\d+)$" [faceId] -> GeoMeshView (HeightBestEffort true, Some (System.Int32.Parse faceId), true, true)  |> NewRenderMode |> PartialMatch
    | "xn1" -> IcosaView (HeightBestEffort true)  |> NewRenderMode |> ExactMatch
    | "c" -> { colours = TectonicColours None; wireframeConnections = true } 
                             |> ClusterView |> NewRenderMode |> ExactMatch
    | "d" -> { colours = TectonicColours None; wireframeConnections = false }
                             |> ClusterView |> NewRenderMode |> ExactMatch


    | "vv" ->(JustBorder, None) |>  BorderView |> NewRenderMode |> ExactMatch
    | "vf" ->(LocalCoordinates, None) |>  BorderView |> NewRenderMode |> ExactMatch
    | "vb" ->(JustBorder, (Some -2)) |>  BorderView |> NewRenderMode |> ExactMatch
    | "vc" ->(JustBorder, (Some -1))|>  BorderView |> NewRenderMode |> ExactMatch
    | ParseRegex "^v(\d+)$" [clusterId] -> 
         (JustBorder ,Some (System.Int32.Parse clusterId))
                             |>  BorderView |> NewRenderMode |> PartialMatch


    | "m" ->                   MercatorView |> NewRenderMode |> ExactMatch
    | "aa" -> Rotate_X false  |> ForceRotate |> UIInstruction |> ExactMatch
    | "aw" -> Rotate_Y true  |> ForceRotate |> UIInstruction |> ExactMatch
    | "aq" -> Rotate_Z true  |> ForceRotate |> UIInstruction |> ExactMatch
    | "ad" -> Rotate_X true |> ForceRotate |> UIInstruction |> ExactMatch
    | "as" -> Rotate_Y false |> ForceRotate |> UIInstruction |> ExactMatch
    | "ae" -> Rotate_Z false |> ForceRotate |> UIInstruction |> ExactMatch
    | "az" ->    Rotate_Stop |> ForceRotate |> UIInstruction |> ExactMatch
    | "ax" ->    Rotate_Stop |> ForceRotate |> UIInstruction |> ExactMatch
    | "[" ->                 ForceEuclidian |> UIInstruction |> ExactMatch
    | "]" ->                 ForceMercator  |> UIInstruction |> ExactMatch 
    | ParseRegex "^[.](.+)" [commandStr] -> commandStr |> parseConsoleCommand |> PartialMatch
    | _ -> NoMatch

  let forceMessage m =
    match m with
    | ExactMatch m' -> m'
    | PartialMatch m' -> m'
    | NoMatch -> NoOp

  let fullMessage pm =
    let msg = forceMessage (partialMessage pm.chars)
    ({chars = ""}, msg)

  let addToMessage pm ch = 
    let newStr = System.String.Concat [pm.chars; string ch]
    match partialMessage newStr with
    | ExactMatch msg -> 
      ({ chars = "" }, Some msg)
    | _ ->
      ({ chars = newStr}, None)

  