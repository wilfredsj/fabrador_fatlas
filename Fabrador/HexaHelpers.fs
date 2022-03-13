module HexaHelpers

open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open System

open RenderTypes

type HexagonData = { row : int; dicol : int; z : float32}

let sqrt3 = Math.Pow(3.0, 0.5) / 2.0 |> float32

let verticesFromHex hex = 

  let scale = 1.0f
  let d1 = 0.5f * scale
  let d2 = sqrt3 * scale

  // up = -d1 + d1 , +d2 + d2  = (0, d2*2)
  
  // dicol = (scale + d1,  d2)
  let mid_y = (float32 hex.row) * d2 * 2.0f + (float32 hex.dicol) * d2
  let mid_x = (float32 hex.dicol) * (d1 + scale)
  let top_z = hex.z
  let z_thickness = 10.0f

  let layer(z) = 
    [
      Vector3(mid_x, mid_y, z);
      Vector3(mid_x + scale, mid_y, z);
      Vector3(mid_x + d1, mid_y + d2, z);
      Vector3(mid_x - d1, mid_y + d2, z);
      Vector3(mid_x - scale, mid_y, z);
      Vector3(mid_x - d1, mid_y - d2, z);
      Vector3(mid_x + d1, mid_y - d2, z)
    ]
  layer(top_z) @ layer(top_z - z_thickness)
  
let coloursFromHex hex = 
  let layer(z) = 
    [
      Vector3(0.4f, 0.4f, 1.0f);
      Vector3(0.4f, 0.34f, 0.8f);
      Vector3(0.4f, 0.34f, 0.8f);
      Vector3(0.4f, 0.24f, 0.5f);
      Vector3(0.4f, 0.4f, 1.0f);
      Vector3(0.4f, 0.24f, 0.5f);
      Vector3(0.4f, 0.4f, 1.0f);
    ]
  layer(0) @ layer(-1)

let normalsFromHex hex = 

  let scale = 1.0f
  let d1 = 0.5f * scale
  let d2 = sqrt3 * scale

  // up = -d1 + d1 , +d2 + d2  = (0, d2*2)
  
  // dicol = (scale + d1,  d2)
  let mid_y = (float32 hex.row) * d2 * 2.0f + (float32 hex.dicol) * d2
  let mid_x = (float32 hex.dicol) * (d1 + scale)
  let top_z = hex.z
  let z_thickness = 10.0f

  let layer(z) = 
    [
      Vector3(0.f, 0.f, 1.0f)
      Vector3(mid_x + scale, mid_y, z);
      Vector3(mid_x + d1, mid_y + d2, z);
      Vector3(mid_x - d1, mid_y + d2, z);
      Vector3(mid_x - scale, mid_y, z);
      Vector3(mid_x - d1, mid_y - d2, z);
      Vector3(mid_x + d1, mid_y - d2, z)
    ]
  layer(top_z) @ layer(top_z - z_thickness)

let vertexIndicesFromHex (i, hex) =
  let offset = 14 * i
  [
    0; 1; 2;
    0; 2; 3;
    0; 3; 4;
    0; 4; 5;
    0; 5; 6;
    0; 6; 1;

    7; 8; 9;
    7; 9; 10;
    7; 10; 11;
    7; 11; 12;
    7; 12; 13;
    7; 13; 8;

    1; 8; 9;
    1; 9; 2;
    2; 9; 10;
    2; 10; 3;
    3; 10; 11;
    3; 11; 4;
    4; 11; 12;
    4; 12; 5;
    5; 12; 13;
    5; 13; 6;
    6; 13; 8;
    6; 8; 1;
  ] |> List.map(fun i -> i + offset)


let hexAsPrimitive hexList = 

  let positionVboData = hexList |> List.collect(verticesFromHex) |> Array.ofList
  let coloursVboData = hexList |> List.collect(coloursFromHex) |> Array.ofList


  let indicesVboData = hexList |> List.indexed |> List.collect(vertexIndicesFromHex) |> Array.ofList 
  
  { vertices = positionVboData; colours = coloursVboData; indices = indicesVboData; primitiveType = "Triangles" }