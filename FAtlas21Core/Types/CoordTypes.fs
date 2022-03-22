namespace FAtlas

module CoordTypes = 
  type Coordinate = { latitude : float; longitude : float}
  type Cartesian = { x : float; y : float; z : float } with
    static member (+) (l : Cartesian, r : Cartesian) =
      { x = (l.x + r.x); y = (l.y + r.y); z = (l.z + r.z) } 
    static member (-) (l : Cartesian, r : Cartesian) =
      { x = (l.x - r.x); y = (l.y - r.y); z = (l.z - r.z) } 
    static member (*) (l : Cartesian, s : float) =
      { x = (l.x * s); y = (l.y * s); z = (l.z * s) } 
    static member (/) (l : Cartesian, s : float) =
      { x = (l.x / s); y = (l.y / s); z = (l.z / s) } 