namespace FAtlas

open CoordTypes
open TriangleMeshTypes
open TectonicTypes

module GeoMeshTypes =
  
  type GeoMeshDatum = { location : Coordinate; r : float; rVol : float }

  type GeoMeshCoord = KeyedPoint<GeoMeshDatum>

  type GeoDivisionState<'Key when 'Key : comparison> = {
    geoParams : TectonicParams
    triangleSet : TriangleSet<KeyedPoint<GeoMeshDatum>>;
    tectData : TectonicData<'Key>
  }