namespace FAtlas

open TriangleTypes
open CoordTypes
open CoordFunctions
open ColourTypes
open TriangleMeshTypes

module TriangleMeshToRender =

  let altColourMaker colourer vtx1d2d (points : 'A array array) k =
    let (i,j) = vtx1d2d |> Map.find(k)
    colourer (i,j) points.[i].[j]

  let tempToColour t =
    let c = 270.0 * (t - 1.0)
    let c' = if c < 0.0 then 0.0 else c
    let hue = 1.6 * System.Math.PI * exp(c' / -20.0) |> float32
    { hue = hue; sat = 0.8f; value = 0.8f } |> makeRGB
  

  let tempColourMaker vtx1d2d (points : TrianglePoint array array) k =
    let (i,j) = vtx1d2d |> Map.find(k)
    let h = points.[i].[j].lowData.heat
    tempToColour h

  let tempAndAltColourMaker vtx1d2d (points : TrianglePoint array array) k =
    let (i,j) = vtx1d2d |> Map.find(k)
    let h' = points.[i].[j] |> interpolateHeat
    tempToColour h'

  let vertexMaker v3 asCart vtx1d2d (points : 'A array array) (a : Cartesian, b : Cartesian, c : Cartesian) n k =
    let (i,j) = vtx1d2d |> Map.find(k)
    let vertex = asCart (i,j) points.[i].[j]
    v3 (vertex.x |> float32, vertex.y |> float32, vertex.z |> float32)

  let triangleToArrays v3 asCart colourer vertexConverters context (triangle : SingleTriangle<'A>) = 
    let n = triangle.points.Length
    let nn1 = ((n + 1) * n) / 2
    let myColourMaker = altColourMaker colourer vertexConverters.vertices1dTo2d triangle.points
    let myVMaker = vertexMaker v3 asCart vertexConverters.vertices1dTo2d triangle.points context n
    let verticesIndices =
      [ 0 .. (n - 2) ]
      |> List.collect(fun i ->
        [ 0 .. (n - (2 + i)) ]
        |> List.collect(fun j ->
          let first = [(i,j); (i+1,j);(i,j+1)]
          if i > 0 then
            let second = [(i,j); (i,j+1); (i-1,j+1)]
            first @ second
          else
            first))
      |> List.map(fun pr -> vertexConverters.vertices2dTo1d |> Map.find(pr))

    let vertices = Array.init(nn1) myVMaker
    let colours = Array.init(nn1) myColourMaker
    let vertexIndices = Array.ofList(verticesIndices)
    (vertices, colours, vertexIndices)

  let asArrays v3 asCart colourer vertexConverters frame triangle = 
    let context = TriangleMeshTypes.extractContext frame triangle
    triangleToArrays v3 asCart colourer vertexConverters context triangle