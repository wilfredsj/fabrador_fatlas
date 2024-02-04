namespace FAtlas

open ConsoleTypes

module ConsoleGridFunctions =
  
  let resetAnsiEscapeColour =
    "\x1b[0m"

  let floatToAnsiEscapeColour (f' : float) =
    //assume f between 0 and 1
    let f = min 1.0 <| max 0.0 f'
    let r = 5 - int (f * 5.0)
    let g = int (f * 5.0)
    let b = 0
    let colour = 16 + b + 6 * g + 36 * r
    sprintf "\x1b[38;5;%im" colour

  let cellToArray (cell : ConsoleCell) = 
    match cell.escapeText, cell.postText  with
    | (Some escape, Some postText) -> [| escape; string cell.text; postText |]
    | (Some escape, None) -> [| escape; string cell.text |]
    | (None, Some postText) -> [| string cell.text; postText |]
    | (None, None) -> [| string cell.text |]

  let lineToString (line : ConsoleLine) =
    line
    |> Array.collect cellToArray
    |> String.concat ""

  let printGrid (grid : ConsoleGrid) printer =
    grid.lines
    |> Array.iter (fun line -> printer (lineToString line))
    printer resetAnsiEscapeColour

  let emptyLine (width : int) =
    Array.init width (fun _ -> { text = ' '; escapeText = None; postText = None })

  let writeStringIntoGrid (grid : ConsoleGrid) (row : int) (col : int) (str : string) (escapeString : string option) =
    if row < 0 then 
      grid
    else
      let grid' =
        if row >= grid.lines.Length then
          let extraRows = Array.init (row - grid.lines.Length + 1) (fun _ -> emptyLine grid.width)
          { lines = Array.append grid.lines extraRows; width = grid.width }
        else
          grid
      let line = grid'.lines.[row]
      let strArray = str.ToCharArray()
      let N = strArray.Length
      let postTextOpt = if Option.isSome escapeString then Some resetAnsiEscapeColour else None
      let preTextOpt = if Option.isSome escapeString then escapeString else Some resetAnsiEscapeColour 
      for i = 0 to N - 1 do
        if col + i >= 0 && col + i < grid.width then
          line.[col + i] <- 
            { 
              text = strArray.[i]; 
              escapeText = if i = 0 then preTextOpt else None; 
              postText = if i = N - 1 then postTextOpt else None }
        else
          ()
      grid'.lines.[row] <- line
      grid'

  let writeStringsIntoGrid grid (writes : ConsoleGridWrite list) =
    writes
    |> List.fold (fun acc write -> writeStringIntoGrid acc write.row write.col write.str write.escapeString) grid

  
  let sampleGridUse printer =
    let grid = defaultConsoleGrid ()

    let instructions = 
      [
        gridWrite 0 4 "Hello" None;
        gridWrite 0 6 "HELLO" (floatToAnsiEscapeColour 0.8 |> Some);
        gridWrite 0 10 "Hello" None;
        gridWrite 1 3 "Hello" None
      ]
    let grid = writeStringsIntoGrid grid instructions
    printer "--Test--"
    printGrid grid printer
    printer "--Test--"

    