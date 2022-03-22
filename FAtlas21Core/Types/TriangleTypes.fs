namespace FAtlas

open CoordTypes
open TriangleMeshTypes

module TriangleTypes = 
  type TriangleId = char

  type ClimateData = { heat : double; northSouth : double; eastWest : double }
  
  type TrianglePoint = { key : (char*int) list; altitude : double;  altitudeVol : double; lowData : ClimateData; highData : ClimateData }
  let interpolateHeat tp = 
    let h1 = tp.lowData.heat
    let h2 = tp.highData.heat
    let a = tp.altitude
    if a > 0.0 then
      if a < 5.0 then
        ((5.0 - a) * h1 + a * h2) / 5.0
      else
        h2
    else
      h1

  type BorderLookup = Map<(char*int) list, TrianglePoint>
  
  let sideLength N = N+1

  let makeEmptyTriangle keys N =
    let side = sideLength N
    { keys = keys; scale = N; points = Array.init side (fun i -> Array.empty<TrianglePoint>)}

  let frameFromPoints keys N points =
    let side = sideLength N
    { keys = keys; scale = N; points = Array.init side (fun i -> Array.init (side - i) (fun j -> points |> Map.find((i,j)))) }
    
  
  let nullBorder = Map.empty<(char*int) list, TrianglePoint>

  let triangleToTemperature p =
    let h1 = p.lowData.heat
    let h2 = p.highData.heat
    let a = p.altitude
    if a > 0.0 then
      if a < 5.0 then
        ((5.0 - a) * h1 + a * h2) / 5.0
      else
        h2
    else
      h1
    
    

