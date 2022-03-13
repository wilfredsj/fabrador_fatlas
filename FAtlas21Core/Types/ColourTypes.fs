module ColourTypes

type HSV = { hue : float32; sat : float32; value : float32 }

let makeHSV (x,y,z) =
  let all = [x;y;z]
  let fpi = System.Math.PI |> float32
  let minValue = all |> List.min
  let maxValue = all |> List.max
  let delta = maxValue - minValue
  if delta > 0.0f then
    let sat = (delta / maxValue);
    let hue = 
      if maxValue = x then
        (y - z) / delta
      elif maxValue = y then
        2.0f + (z  - x) / delta
      else 
        4.0f + (x - y) / delta

    //Hue range is now [-1,5], normalise to [0, 2pi]
    let finalHue = 
      if hue < 0.0f then
        (hue * fpi / 3.0f) + fpi * 2.0f
      else 
        (hue * fpi / 3.0f) 

    if finalHue > 2.0f * fpi then
      printfn "OOB"
    else 
      ()
    
    { value = maxValue;  sat = sat; hue = finalHue  }
  else
    { value = maxValue;  sat = 0.0f; hue = -1.0f  }

let makeRGB hsv =
  let fpi = System.Math.PI |> float32
  let (r,g,b) = 
    if hsv.sat = 0.0f then
      (hsv.value, hsv.value, hsv.value)
    else 
      let delta = hsv.sat * hsv.value
      let hbar = hsv.hue / (fpi / 3.0f)
      let sector = hbar |> int
      let frac = hbar - float32 sector
      match sector with
      | 0 -> 
          //R > G > B
          let r = hsv.value
          let b = (1.0f - hsv.sat) * hsv.value
          let g = (r - b) * frac + b
          (r,g,b)
      | 1 ->
          //G > R > B
          let g = hsv.value;
          let b = (1.0f - hsv.sat) * hsv.value;
          let r = (g - b) * (1.0f - frac) + b
          (r,g,b)
      | 2 ->
          //G > B > R
          let g = hsv.value;
          let r = (1.0f - hsv.sat) * hsv.value;
          let b = (g - r) * frac + r
          (r,g,b)
      | 3 -> 
          //B > G > R
          let b = hsv.value;
          let r = (1.0f - hsv.sat) * hsv.value;
          let g = (b - r) * (1.0f - frac) + r
          (r,g,b)
      | 4 -> 
          //B > R > G
          let b = hsv.value;
          let g = (1.0f - hsv.sat) * hsv.value;
          let r = (b - g) * frac + g
          (r,g,b)
      | 5 ->
          //R > B > G
          let r = hsv.value;
          let g = (1.0f - hsv.sat) * hsv.value;
          let b = (r - g) * (1.0f - frac) + g
          (r,g,b)
      | i -> failwith <| sprintf "Unknown HSV sector %i" i
  (r,g,b)

let floatToFullSat scale shift input = 
  System.Math.Atan((input - shift) / scale)
  |> fun x -> 0.5 + x / System.Math.PI
  |> float32
  |> fun hue -> { hue = hue; sat = 1.0f; value = 1.0f }

let rangeWithSatValue sat value minvalue maxvalue input = 
  let x = (input - minvalue) / (maxvalue - minvalue)
  let xbounded = min 0.999 <| max 0.0 x
  { hue = 2.0 * System.Math.PI * xbounded |> float32; sat = sat; value = value }

let rangeToFullSat = rangeWithSatValue 1.0f 1.0f


