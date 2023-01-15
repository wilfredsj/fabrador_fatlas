namespace FAtlas

module MathUtils = 
  let correlate rho u v =
    (u, rho * u + v * sqrt (1.0-rho*rho))

