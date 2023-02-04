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

  let vertexMaker v3 asCart vtx1d2d (points : 'A array array) k =
    let (i,j) = vtx1d2d |> Map.find(k)
    let vertex = asCart (i,j) points.[i].[j]
    v3 (vertex.x |> float32, vertex.y |> float32, vertex.z |> float32)

  let vertexMakerForHex v3 asCart vtx1d2dLogical vtx1d2dPhysical (points : 'A array array) k =
    let (l_i,l_j) = vtx1d2dLogical |> Map.find(k)
    let ((p1_i,p1_j),(p2_i, p2_j)) = vtx1d2dPhysical |> Map.find(k)
    let vertex = asCart (l_i,l_j) points.[l_i].[l_j] points.[p1_i].[p1_j] points.[p2_i].[p2_j]
    v3 (vertex.x |> float32, vertex.y |> float32, vertex.z |> float32)

  let triangleToArrays v3 asCart colourer vertexConverters (triangle : SingleTriangle<'A>) = 
    let n = triangle.points.Length
    let nn1 = ((n + 1) * n) / 2
    let myColourMaker = altColourMaker colourer vertexConverters.vertices1dTo2d triangle.points
    let myVMaker = vertexMaker v3 asCart vertexConverters.vertices1dTo2d triangle.points
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

    let vertices = Array.init nn1 myVMaker
    let colours = Array.init nn1 myColourMaker
    let vertexIndices = Array.ofList(verticesIndices)
    (vertices, colours, vertexIndices)


  // N.B.
  //   From edge-divided triangle
  //     there is not exact tiling onto hex
  //       (see below, k=2)
  //          04
  //         /  \
  //        03 - 13         n = 5
  //       /  \ /  \          n np1 /2 = 15
  //      02 - 12 - 22        n nm1 / 2 = 10
  //     /  \ /  \ /  \     m = 6 *nn1/2 - 3 * 2 * n
  //    01 - 11 - 21 - 31      = 90 - 30
  //   /  \ /  \ /  \ /  \     = 60
  //  00 - 10 - 20 - 30 - 40

  
  //          0
  //         / \
  //        0 - 0      n = 3
  //       / \ / \      nn1/2 = 6
  //      0 - 0 - 0    m = 18 = 6 * nn1 - 3 * 2 * n

  //   So any "hex" grid must in fact be effectively 
  //     a render of the "dual" graph
  //     i.e. an edge of the triangle set is a vertex of the hex grid
  //             vertex -> face
  //             face -> edge
  //     and then some stylisation such that those "vertices" have different
  //             height
  //             colour 
  //         when included as part of a certain face or other one
  //         which in GL terms mean will be a seperate vertex in each instance
  
  let triangleToArraysFakeHex v3 asCart colourer (hexConverter : HexVertexConverters) (triangle : SingleTriangle<'A>) = 
    let n = triangle.points.Length
    let myColourMaker = altColourMaker colourer hexConverter.vertices1dTo2dLogical triangle.points
    let myVMaker = vertexMakerForHex v3 asCart hexConverter.vertices1dTo2dLogical hexConverter.vertices1dTo2dPhysical triangle.points
    let verticesIndices =
      [ 0 .. (n - 1) ]
      |> List.collect(fun i ->
        [ 0 .. (n - (1 + i)) ]
        |> List.collect(fun j ->
          //   +- ...  +j
          //  /  \ms2 /    \
          // /ms2 \  / mf   \
          //i- --- ij  ...  i+
          // \ mb /  \ ms1 /
          //  \  /ms1 \   /
          //   -j ...  -+
          let maybeForwards =
            if (i + j) < (n - 1) then 
              [(i,j); (i+1,j);(i,j+1)]
              |> List.map(fun ij -> (i,j),ij)
            else
              []
          let maybeSideways1 = 
            if i > 0 then
              let others =
                if (i+j) < (n-1) then
                  [(i,j); (i,j+1); (i-1, j+1); (i,j); (i-1, j+1); (i-1, j)]
                else
                  [(i,j); (i-1, j+1); (i-1, j)]
              others
              |> List.map(fun ij -> (i,j),ij)
            else
              []
          let maybeSideways2 = 
            if j > 0 then
              let others =
                if (i+j) < (n - 1) then 
                  [(i,j); (i,j-1); (i+1, j-1); (i,j); (i+1, j-1); (i+1, j)]
                else
                  [(i,j); (i,j-1); (i+1, j-1)]
              others
              |> List.map(fun ij -> (i,j),ij)
            else
              []
          let maybeBackwards = 
            if (i > 0) && (j > 0) then
              [(i,j); (i-1,j); (i,j-1)]
              |> List.map(fun ij -> (i,j),ij)
            else
              []
          let res = maybeForwards @ maybeSideways1 @ maybeSideways2 @ maybeBackwards
          res
          ))
      |> List.map(fun pr -> hexConverter.vertices2dPhysicalTo1d |> Map.find(pr))

    let vertices = Array.init hexConverter.numEntries myVMaker
    let colours = Array.init hexConverter.numEntries myColourMaker
    let vertexIndices = Array.ofList(verticesIndices)
    (vertices, colours, vertexIndices)

  let asArrays v3 asCart colourer vertexConverters triangle = 
    triangleToArrays v3 asCart colourer vertexConverters triangle