namespace FAtlas

open ColourTypes

module ViewUtilityFunctions =
    
  let uniformHue max mkColour i ij k = rangeToFullSat 0.0 max (float i) |> makeRGB |> mkColour
  let uniformHue2 sat value max mkColour i ij k = rangeWithSatValue sat value 0.0 max (float i) |> makeRGB |> mkColour
  let uniformHueSat sat value max mkColour x = rangeWithSatValue sat value 0.0 max x |> makeRGB |> mkColour
  let uniformHueSat2 sat value max mkColour x = rangeWithSatValue sat value 0.0 max x 
  let singleHueVarySat (hue : float) (value : float) scale mkColour x =
    let sat = (2.0 / System.Math.PI) * atan (x / scale)
    { hue = hue |> float32; sat = sat |> float32 ; value = value |> float32}
    |> makeRGB
    |> mkColour