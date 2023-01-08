namespace FAtlas

open TriangleMeshFunctions
open TectonicTypes
open TriangleMeshTypes
open CoordFunctions
open CoordTypes
open TectonicFunctions

module TectonicFunctionsNonProd =

  type OneSidedBoundarySection = {
    x_lower_os : float
    y_lower_os : float
    dy_dx_os   : float option
  }
  type NormalizedBoundarySection2 = {
    x : float
    y_left : float
    rhs_stack : (float*float) list
  }
  type ClusterBoundary2 = { 
    ref : Cartesian; 
    hub : Cartesian; 
    pts : ClusterBoundaryPoint list 
    normalizedBoundary2 : NormalizedBoundarySection2 array
  }

  let truncate nbs max_x =
    if max_x >= nbs.x_upper then
      failwith <| sprintf "Bad normalization %f >= max, elt=%A" max_x nbs
    else
      { x_lower = nbs.x_lower; x_upper = max_x; 
        y_lower = nbs.y_lower; y_upper = yInterp nbs max_x;
        dy_dx = nbs.dy_dx }

  let split nbs mid_x =
    if mid_x >= nbs.x_upper then
      failwith <| sprintf "Bad normalization (i) %f outside bounds, elt=%A" mid_x nbs
    elif mid_x <= nbs.x_lower then
      failwith <| sprintf "Bad normalization (ii) %f outside bounds, elt=%A" mid_x nbs
    else
      let mid_y = yInterp nbs mid_x
      let left=
        { x_lower = nbs.x_lower; x_upper = mid_x; 
          y_lower = nbs.y_lower; y_upper = mid_y;
        dy_dx = nbs.dy_dx }
      let right =
        { x_lower = mid_x; x_upper = nbs.x_upper; 
          y_lower = mid_y; y_upper = nbs.y_upper;
        dy_dx = nbs.dy_dx }
      (left, right)

  let updateStackWithNewDyDx ll (y,dy) =
    let l2 = 
      ll
      |> List.filter(fun (y1,dy1) -> (y1 > y) || (dy1 > dy))
    let l1 = 
      if List.exists(fun (y1,dy1) -> (y1 > y) && (dy1 > dy)) ll then
        []
      else
        [(y, dy)]
    l1 @ l2

  let interpolated2 (nbs : NormalizedBoundarySection2) x =
    nbs.rhs_stack
    |> List.map(fun (y_right, dy_dx) -> (y_right + (x - nbs.x) * dy_dx)) 
    |> fun ys -> 
        match ys with 
        | [] -> None
        | x :: _ -> Some (List.max ys)

  let updateStackToNewX (nbs : NormalizedBoundarySection2) x =
    nbs.rhs_stack
    |> List.map(fun (y_right, dy_dx) -> (y_right + (x - nbs.x) * dy_dx), dy_dx)

  let getInterceptTime (yBig : float) dyBig ySmall (dySmall : float) =
    if yBig < ySmall then
      failwith "Logic error"
    else
      let dy = yBig - ySmall
      let ddy = dySmall - dyBig
      if ddy < 0.0 then
        None
      else
        Some <| dy / ddy

  let getNextStackEventBeforeX (nbs : NormalizedBoundarySection2) x =
    if nbs.rhs_stack.IsEmpty then 
      None
    else 
      let (yBig, dyBig) = nbs.rhs_stack |> List.maxBy(fst)
      let rem = nbs.rhs_stack |> List.filter(fun y' -> y' <> (yBig, dyBig))
      let is = 
        rem 
        |> List.collect(fun (ySmall, dySmall) -> getInterceptTime yBig dyBig ySmall dySmall |> Option.toList)
        |> List.filter(fun x' -> x' < x)

      if is.IsEmpty then
        None
      else 
        is |> List.max |> Some 



  let insertNormalizedSection3 (sortedList : OneSidedBoundarySection list) (inputElt : NormalizedBoundarySection) =
    // Find left to insert
    // every existing element from left to right, check if new value is > existing
    // every existing segment from left to right, check if will be intercept before next element
    match sortedList with
    | [] -> 
      let first = { x_lower_os = inputElt.x_lower; y_lower_os = inputElt.y_lower; dy_dx_os = Some(inputElt.dy_dx) }
      let second = { x_lower_os = inputElt.x_upper; y_lower_os = inputElt.y_upper; dy_dx_os = None }
      [first; second]
    | _ -> 
      let sortedPairs = sortedList |> List.pairwise
      //let seekLeftSorted 
      failwith "Not implemented"


  let insertNormalizedSection2 (sortedList : NormalizedBoundarySection2 list) (inputElt : NormalizedBoundarySection) =

    let rec insertRight accOutput lastElt last (normalizedListIn : NormalizedBoundarySection2 list) newElt =
      match normalizedListIn with
      | [] -> 
        { x = newElt.x_upper; y_left = newElt.y_upper; rhs_stack = []} :: accOutput
      | sElt :: tail ->
        if sElt.x > newElt.x_upper then
          // Right location to insert the new element
          let interpolatedY = interpolated2 lastElt newElt.x_upper
          if Option.exists(fun y -> y > newElt.y_upper) interpolatedY then
            // Previous element did define RHS and is larger than this element's value
            // and this element does not have RHS so no delayed effect
            (List.rev normalizedListIn) @ accOutput
          else
            // Either:
            //   previous element did not define RHS - how is that possible, we are closing an interval?
            //   or 
            //   previous element defined RHS but its interpolatedY is lower than y
            //     and this element does NOT have RHS so we don't need to add anything
            let newDydxOpt = updateStackToNewX lastElt newElt.x_upper
                          
            let newElt' = { x = newElt.x_upper; y_left = newElt.y_upper; rhs_stack = newDydxOpt }
            (List.rev normalizedListIn) @ newElt' :: accOutput
        else 
          // Not the right place to insert the new element
          // However since left element already planted, need to check if old element should be replaced by this one
          let interpolatedY = interpolated newElt sElt.x
          if interpolatedY > sElt.y_left then
            // New value is higher than the old one.
            // And new element will define RHS...
            //  need to merge the stack of RHS values.
            let modElt = { x = sElt.x; y_left = interpolatedY; rhs_stack = updateStackWithNewDyDx sElt.rhs_stack (interpolatedY, newElt.dy_dx) }
            
            // *AND* get next date of change
            failwith "TODO"

            insertRight (modElt :: accOutput) modElt true tail newElt
          else
            //if sElt.rhs_y_dydx.IsNone then
            //  let modElt = { x = sElt.x; y_left = sElt.y_left; rhs_y_dydx = Some((interpolatedY, newElt.dy_dx)) }
            //  insertRight (modElt :: accOutput) modElt true tail newElt
            //else 
            //  insertRight (sElt :: accOutput) sElt false tail newElt
            failwith "Not implemented"
    
    let rec insertLeft accOutput lastEltOpt last (normalizedListIn : NormalizedBoundarySection2 list) newElt =
      match normalizedListIn with
      | [] -> 
        let newElt' = { x = newElt.x_lower; y_left = newElt.y_lower; rhs_stack = [(newElt.y_lower, newElt.dy_dx)] }
        insertRight (newElt' :: accOutput) newElt' true [] newElt
      | sElt :: tail ->
        if sElt.x > newElt.x_lower then
          match lastEltOpt with
          | Some lastElt -> 
            let interpolatedY = interpolated2 lastElt newElt.x_lower
            if Option.exists(fun y -> y > newElt.y_lower) interpolatedY then
              // Interpolated y value from previous is higher than this
              insertRight accOutput sElt false normalizedListIn newElt
            else
              //let newElt' = { x = newElt.x_lower; y_left = newElt.y_lower; rhs_y_dydx = Some (newElt.y_lower, newElt.dy_dx) }
              //insertRight (newElt' :: accOutput) newElt' true tail newElt
              failwith "Not implemented"
          | None -> 
            //let newElt' = { x = newElt.x_lower; y_left = newElt.y_lower; rhs_y_dydx = Some (newElt.y_lower, newElt.dy_dx) }
            //insertRight (newElt' :: accOutput) newElt' true tail newElt
            failwith "Not implemented"
        else 
          insertLeft (sElt :: accOutput) (Some sElt) false tail newElt
    insertLeft [] None false sortedList inputElt

  let insertNormalizedSection sortedList inputElt =
    // This is probably flawed.
    // Alternative idea:
    //   Keep sections indexed by lower, only
    let rec seekForLeft accOutput normalistedIn newElt =
      match normalistedIn with
      | [] -> newElt :: accOutput
      | sElt :: tail ->
        if newElt.x_upper <= sElt.x_lower then
          // new  |l...r|
          // sort         |x.l x.r|
          (List.rev normalistedIn) @ [sElt] @ accOutput

        // Needs to be '<' since 'split' will throw if sElt.lower hits a bound
        elif newElt.x_lower < sElt.x_lower then
          // new  |l  ... r|
          // sort    |x.l   x.r|
          let (newPart, clashPart) = split newElt sElt.x_lower
          seekForLeft (newPart :: accOutput) normalistedIn clashPart
        elif sElt.x_upper <= newElt.x_lower then
          //             |l...r|
          //   |x.l x.r|
          seekForLeft (sElt :: accOutput) tail newElt
        elif sElt.x_lower = newElt.x_lower then
          // Needs to be '<' since 'split' will throw if sElt.x_upper hits a bound
          if sElt.x_upper < newElt.x_upper then
            // | l   ...   r |
            // | l ...  r |
            let y_interp_right = yInterp newElt sElt.x_upper
            if sElt.y_lower < newElt.y_lower then
              if sElt.y_upper < y_interp_right then
                let (second, rem) = split newElt sElt.x_upper
                seekForLeft (second :: accOutput) tail rem
              else 
                let dy0 = newElt.y_lower - sElt.y_lower // >0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (old_ab, old_c) = split sElt intercept

                let (new_b, new_c) = split newElt intercept
                let (_, rem) = split new_c sElt.x_upper

                //         new_b    old_c
                //         old_b         new_c
                seekForLeft (old_c :: new_b ::  accOutput) tail rem
            else              
              if sElt.y_upper < y_interp_right then
                //               new_c new_d
                //               old_c
                //         old_b         
                //         new_b
                let dy0 = newElt.y_lower - sElt.y_lower // <0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let old_b = truncate sElt intercept
                let (_, new_cd) = split newElt intercept
                let (new_c, rem) = split new_cd sElt.x_upper
                seekForLeft (new_c :: old_b :: accOutput) tail rem
              else
                
                //  old_b old_c
                //  new_b new_c new_d
                let (_, rem) = split newElt sElt.x_upper
                seekForLeft (sElt :: accOutput) tail rem
          else
            // | l ... r |
            // | l ...     r |
            // sElt.x_upper >= newElt.x_upper 
            let y_interp_right = yInterp sElt newElt.x_upper
            if sElt.y_lower < newElt.y_lower then
              if y_interp_right < newElt.y_upper then
                // new_ab 
                // old_ab old_c
                if sElt.x_upper = newElt.x_upper then
                  (List.rev tail) @ newElt :: accOutput
                else
                  let (_, old_c) = split sElt newElt.x_upper
                  (List.rev tail) @ old_c :: newElt :: accOutput
              else
                let dy0 = newElt.y_lower - sElt.y_lower // >0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (_, old_c) = split sElt intercept

                let (new_ab, _) = split newElt intercept
                //         new_ab         old_c
                //         old_ab   new_c
                //  
                (List.rev tail) @ old_c :: new_ab :: accOutput
            else
              if y_interp_right < newElt.y_upper then
                //               new_c old_d
                //               old_c
                //         old_ab         
                //         new_ab
                let dy0 = newElt.y_lower - sElt.y_lower // <0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (old_ab, old_cd) = split sElt intercept
                let (_, new_c) = split newElt intercept

                if old_cd.x_upper = newElt.x_upper then
                  (List.rev tail) @ new_c :: old_ab :: accOutput
                else
                  let (_, old_d) = split old_cd newElt.x_upper
                  (List.rev tail) @ old_d :: new_c :: old_ab :: accOutput
              else
                (List.rev tail) @ newElt :: accOutput
        else
          // Needs to be '<' since 'split' will throw if sElt.x_upper hits a bound
          if sElt.x_upper < newElt.x_upper then
            //         |l ...  r|      (newElt)
            //   |x.l   x.r|           (sElt)
            let y_interp_left = yInterp sElt newElt.x_lower
            let y_interp_right = yInterp newElt sElt.x_upper
            if y_interp_left < newElt.y_lower then
              if sElt.y_upper < y_interp_right then
                //        new_b  new_c
                // old_a  old_b
                let old_a = truncate sElt newElt.x_lower
                let (new_b, new_c) = split newElt sElt.x_upper
                seekForLeft (new_b :: old_a :: accOutput) tail new_c
              else
                let dy0 = newElt.y_lower - y_interp_left // >0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (old_ab, old_c) = split sElt intercept
                let old_a = truncate old_ab newElt.x_lower

                let (new_b, new_c) = split newElt intercept
                let (_, rem) = split new_c sElt.x_upper

                //         new_b    old_c
                //         old_b         new_c
                //  old_a
                //  
                seekForLeft (old_c :: new_b :: old_a :: accOutput) tail rem
            else //i.e. y_interp_left >= newElt.y_lower 
              if sElt.y_upper < y_interp_right then
                //               new_c new_d
                //               old_c
                //         old_b         
                //         new_b
                //  old_a
                //  
                let dy0 = newElt.y_lower - y_interp_left // <0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let old_ab = truncate sElt intercept
                let (_, new_cd) = split newElt intercept
                let (new_c, rem) = split new_cd sElt.x_upper
                seekForLeft (new_c :: old_ab :: accOutput) tail rem
              else
                
                //  old_a old_b olc_c
                //        new_b new_c new_d
                let (_, rem) = split newElt sElt.x_upper
                seekForLeft (sElt :: accOutput) tail rem
          else // sElt.x_upper >= newElt.x_upper 
            // newElt      |l ...  r|
            // sElt    |x.l          x.r|
            let y_interp_left = yInterp sElt newElt.x_lower
            let y_interp_right = yInterp sElt newElt.x_upper
            if y_interp_left < newElt.y_lower then
              if y_interp_right < newElt.y_upper then
                //        new_b  
                // old_a  old_b old_c
                let (old_a, old_bc) = split sElt newElt.x_lower
                if old_bc.x_upper = newElt.x_upper then
                  (List.rev tail) @ newElt :: old_a :: accOutput
                else
                  let (_, old_c) = split old_bc newElt.x_upper
                  (List.rev tail) @ old_c :: newElt :: old_a :: accOutput
              else
                let dy0 = newElt.y_lower - y_interp_left // >0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // <0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (old_ab, old_c) = split sElt intercept
                let old_a = truncate old_ab newElt.x_lower

                let (new_b, _) = split newElt intercept
                //         new_b         old_c
                //         old_b   new_c
                //  old_a
                //  
                (List.rev tail) @ old_c :: new_b :: old_a :: accOutput
            else
              if y_interp_right < newElt.y_upper then
                //               new_c old_d
                //               old_c
                //         old_b         
                //         new_b
                //  old_a
                let dy0 = newElt.y_lower - y_interp_left // <0
                let d_dy_dx = newElt.dy_dx - sElt.dy_dx // >0
                let dx = -dy0 / d_dy_dx
                let intercept = newElt.x_lower + dx
                let (old_ab, old_cd) = split sElt intercept
                let (_, new_c) = split newElt intercept
                if old_cd.x_upper = newElt.x_upper then
                  (List.rev tail) @ new_c :: old_ab :: accOutput
                else
                  let (_, old_d) = split old_cd newElt.x_upper
                  (List.rev tail) @ old_d :: new_c :: old_ab :: accOutput
              else
                (List.rev tail) @ newElt :: accOutput
    seekForLeft [] sortedList inputElt |> List.rev

  let makeBoundaryData1 points centroid referenceCart : ClusterBoundary2 = 
    let args = 
      points
      |> pairwiseWithCyclic_Reversed None
      |> List.map(fun (x,y) -> (x.argument, y.argument))

    let normBoundary = 
      points
      |> pairwiseWithCyclic_Reversed None
      |> List.fold( 
        fun s (big,small) ->
          let big' =
            if small.argument <= big.argument then
              big
            else
              { argument = big.argument + (2.0 * System.Math.PI); pt = big.pt; inUrl=big.inUrl; outUrl = big.outUrl; radius = big.radius }
          insertNormalizedSection2 s (makeNBS_LR small big')) []
      |> Array.ofList

    { pts = points; hub = centroid; ref = referenceCart; normalizedBoundary2 = normBoundary }

  let lookupBoundary1 boundaryArr argument =
    let size = Array.length boundaryArr
    let arg' =
      if Array.isEmpty boundaryArr then
        failwith "Empty Boundary"
      elif argument < boundaryArr.[0].x_lower then
        argument + (2.0 * System.Math.PI)
      elif argument < boundaryArr.[size-1].x_upper then
        failwith "Boundary missing some bounds"
      else
        argument
    let rec binSearchBdry (boundary : NormalizedBoundarySection array) lower uppern1 current a =
      if a >= boundary.[current].x_lower then
        if a <= boundary.[current].x_upper then
          boundary.[current]
        else
          if current + 1 = uppern1 then
            boundary.[current]
          else
            binSearchBdry boundary current uppern1 ((current + uppern1) / 2) a
      else
        if lower + 1 = current then
          boundary.[current]
        else
          binSearchBdry boundary lower current ((lower + current) / 2) a
    binSearchBdry boundaryArr 0 size (size/2) arg'

  let lookupBoundary2 (boundaryArr : NormalizedBoundarySection2 array) argument =
    let size = Array.length boundaryArr
    let arg' =
      if Array.isEmpty boundaryArr then
        failwith "Empty Boundary"
      elif argument < boundaryArr.[0].x then
        argument + (2.0 * System.Math.PI)
      elif argument < boundaryArr.[size-1].x then
        failwith "Boundary missing some bounds"
      else
        argument
    let rec binSearchBdry2 (boundary : NormalizedBoundarySection2 array) lower uppern1 current a =
      if a >= boundary.[current].x then
        if current + 1 = uppern1 then
          boundary.[current]
        else
          binSearchBdry2 boundary current uppern1 ((current + uppern1) / 2) a
      else
        if lower + 1 = current then
          boundary.[current]
        else
          binSearchBdry2 boundary lower current ((lower + current) / 2) a
    binSearchBdry2 boundaryArr 0 size (size/2) arg'

  let getLocalCoordinates2 cluster localPoint =
    let b = makeBearinger cluster.hub cluster.ref
    let th = b localPoint
    let r = modulus (localPoint - cluster.hub)
    let s = lookupBoundary2 cluster.normalizedBoundary2 th
    let r_actual =  
      match interpolated2 s th with
      | None -> failwith "Bad"
      | Some r' -> r'
    (r / r_actual, th)

