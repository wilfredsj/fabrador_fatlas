namespace FAtlas

open GeoMeshTypes
open TriangleMeshTypes
open TriangleMeshFunctions
open ViewUtilityFunctions

module GeoMeshViewFunctions =


  let actualHeightColours floored (triangleSet : TriangleSet<KeyedPoint<GeoMeshDatum>>) mkColour i ij k =
    let tp = triangleSet.triangles.[i].points.[fst ij].[snd ij]
    let r = tp.datum.r
    if floored && r < 1.0 then
      allGray mkColour i ij k
    else
      continuousPositiveHue 1.0 1.0 1.5 mkColour r
        
      
            