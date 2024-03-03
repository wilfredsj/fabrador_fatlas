namespace FAtlas

open CoordTypes
open TriangleMeshTypes
open TectonicTypes

module GeoMeshTypes =
  
  type GeoMeshDatum = { location : Coordinate; r : float; rVol : float }

  type GeoMeshCoord = KeyedPoint<GeoMeshDatum>

  type GeoConstants = {
    radiusAtEquator : float; // in m
    insolation : float; // in W/m^2
  }

  let defaultGeoConstants = { radiusAtEquator = 6371000.0; insolation = 340.0 }

  type GeoDivisionState<'Key when 'Key : comparison> = {
    geoParams : TectonicParams
    triangleSet : TriangleSet<KeyedPoint<GeoMeshDatum>>;
    tectData : TectonicData<'Key>
  }