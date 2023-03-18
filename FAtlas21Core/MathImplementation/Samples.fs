namespace FAtlas

module Samples =

  let sampleMean<'A> (samples : 'A array) (fn : 'A -> float) = 
    let n = Array.length samples |> float
    samples 
    |> Array.sumBy fn
    |> fun x -> x / n

  let unbiasedSampleVariance<'A> (samples : 'A array) (fn : 'A -> float) = 
    let n = Array.length samples |> float
    let proj = samples |> Array.map(fn)
    let sum_x = proj |> Array.sum 
    let sum_x2 = proj |> Array.sumBy(fun x -> x*x)
    let e_x = (sum_x / n)
    let upscale = n / (n - 1.0)
    upscale * ( (sum_x2 / n) - (e_x * e_x) )

  let unbiasedSampleCovariance<'A> (samples : 'A array) (fnA : 'A -> float) (fnB : 'A -> float) = 
    let n = Array.length samples |> float
    let proj = samples |> Array.map (fun a -> (fnA(a), fnB(a)))
    let sum_xa = proj |> Array.sumBy(fst)
    let sum_xb = proj |> Array.sumBy(snd)
    let sum_xab = proj |> Array.sumBy(fun (x,y) -> x*y)
    let e_xa = (sum_xa / n)
    let e_ya = (sum_xb / n)
    let upscale = n / (n - 1.0)
    upscale * ((sum_xab / n) - (e_xa * e_ya))
    
    
    
  let sample dist n =
    [| 0 .. (n-1) |]
    |> Array.map(fun _ -> dist())
    

