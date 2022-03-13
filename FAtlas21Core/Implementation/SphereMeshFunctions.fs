namespace FAtlas

open CoordTypes
open SphereMeshTypes
open TriangleTypes

open CoordFunctions

module SphereMeshFunctions =

  let mergeKeys (a:CoordinateKeys) (b:CoordinateKeys) =
    let rec mergeKeys' acc a b =
      match (a,b) with
      | (_,[]) -> a @ acc
      | ([],_) -> b @ acc
      | ((ac1, ab1) :: a2, (bc1, bb1) :: b2) ->
        if ac1 = bc1 then
          let acc' = (ac1, ab1+bb1) :: acc
          mergeKeys' acc' a2 b2
        elif ac1 < bc1 then
          let acc' = (ac1, ab1) :: acc  
          mergeKeys' acc' a2 b
        else
          let acc' = (bc1, bb1) :: acc 
          mergeKeys' acc' a b2
    mergeKeys' [] a b |> List.rev

          
        
    
  let centroid (poly : Polygon) =
    match poly with
    | [] -> cart 0.0 0.0 0.0
    | h :: tl -> 
      let myFold (c,i) (elt : Cartesian) = (c+elt, i+1)
      let init = (h,1)
      tl 
      |> List.fold myFold init
      |> fun (agg, mass) -> agg / (float mass)


  //let refineTriangleMesh (layer : LayerBookkeeping) = 
    //1

    
//    let volFactor = 0.2 + alt' |> (*) 0.5 |> System.Math.Abs
//    let alt'' = alt' + (u - 0.5) * (0.25 + vol') * volFactor
//    let vol'' = vol' * (v + 0.5) |> (*) 0.5

  
 
  //let divideTriangleSet rng numDivisions triangleSet =
  //  if numDivisions > 0 then
  //    [| 1 .. numDivisions |] |> Array.fold (fun t i -> singleDivideTriangleSet rng t) triangleSet
  //  else
  //    triangleSet