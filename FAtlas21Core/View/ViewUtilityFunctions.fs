namespace FAtlas

open ColourTypes

module ViewUtilityFunctions =
    
    
  let grayscale mkColour i ij k = ((i * 79) % 71) |> float32 |> fun y -> (y / 71.0f) * 0.4f + 0.1f |> fun z -> (z,z,z) |> mkColour
  let allGray mkColour i ij k = 0.7f |> fun z -> (z,z,z) |> mkColour

  let uniformHue max mkColour i ij k = rangeToFullSat 0.0 max (float i) |> makeRGB |> mkColour
  let uniformHue2 sat value max mkColour i ij k = rangeWithSatValue sat value 0.0 max (float i) |> makeRGB |> mkColour
  let uniformHueSat sat value max mkColour x = rangeWithSatValue sat value 0.0 max x |> makeRGB |> mkColour
  let uniformHueSat2 sat value max mkColour x = rangeWithSatValue sat value 0.0 max x 
  let singleHueVarySat (hue : float) (value : float) scale mkColour x =
    let sat = (2.0 / System.Math.PI) * atan (x / scale)
    { hue = hue |> float32; sat = sat |> float32 ; value = value |> float32}
    |> makeRGB
    |> mkColour

  let continuousPositiveHue (sat : float) (value : float) logBase mkColour (x : float) =
    // -0.5 Blue
    // 0 Red 
    // 1 Green 
    // 2 Blue 
    // 4 Red
    // 8 Green 
    // ...
    
    let hue' = if x <= 0.0 then 0.0 else System.Math.PI * (2.0 / 3.0) * (log x) / (log logBase)
    let hue = 
      if hue' < 0.0 then 
        let n = -hue' / (2.0 * System.Math.PI) |> int 
        hue' + (float (n+1)) * (2.0 * System.Math.PI) 
      else 
        hue'
    { hue = hue |> float32; sat = sat |> float32 ; value = value |> float32}
    |> makeRGB
    |> mkColour

