﻿namespace FAtlas

open CoordTypes
open CoordFunctions

open TriangleMeshTypes
open TriangleMeshFunctions

open TectonicTypes
open TectonicFunctions

open GeoMeshTypes

module GeoMeshFunctions =
  
  let fakeCart gmd =
    cartFromSphere gmd.location

  let actualCart gmd =
    cartFromSphereWithRadius gmd.r gmd.location

  let actualCartFloored gmd =
    cartFromSphereWithRadius (max gmd.r 1.0) gmd.location

  let convertOnePoint correlatedSampler param (td : TectonicData<'A>) scale url (point : KeyedPoint<Coordinate>) =
    let c =
      td.cca.clusterAssignments
      |> Map.find url
    let fakeCart = cartFromSphere point.datum
    let ((bias, stress), _) = getStressedHeightBiasAndStress td.plates.[c-1] fakeCart

    let baseHeight = heightBiasToHeight param bias
    let baseVol = stressToVol param stress

    let (actualHeight, actualVol) = sampleHeightVol correlatedSampler scale param baseHeight baseVol

    let p = {
      location = point.datum
      r = actualHeight
      rVol = actualVol
    }
    {
      datum = p
      key = point.key
    }
    
    
  let interpolateGMD correlatedSampler param scaleInt a b newKey =
    let fscale = float scaleInt
    let midPoint = (fakeCart a.datum + (fakeCart b.datum)) * 0.5
    let baseHeight = (a.datum.r + b.datum.r) * 0.5
    let baseVol = (a.datum.rVol + b.datum.rVol) * 0.5
    let (actualHeight, actualVol) = sampleHeightVol correlatedSampler fscale param baseHeight baseVol
    let p = {
      location = midPoint |> coordFromCart
      r = actualHeight
      rVol = actualVol
    }
    {
      datum = p
      key = newKey
    }

  let divideGeoMesh rng param (gds : GeoDivisionState<'A>) =
    let correlatedSampler = getSampler param rng
    let ts' = divideTriangleSet (interpolateGMD correlatedSampler param) gds.triangleSet
    {
      gds with triangleSet = ts'
    }
    
  let createGeoGridFromBase rng param (td : TectonicData<'A>) =
    let correlatedSampler = getSampler param rng
    let triangleSet = td.cca.meshData
    let newTriangleSets = 
      triangleSet.triangles
      |> Array.indexed
      |> Array.mapFold (
        fun cache (t,ts) ->
          let mutable localCache = cache
          let newPoints = 
            ts.points
            |> Array.mapi(fun i arri ->
              arri
              |> Array.mapi(fun j elt ->
                let url = { t = t; i = i; j = j} |> normalizeElement triangleSet
                if Map.containsKey elt.key localCache then
                  Map.find elt.key localCache
                else
                  let newElt = convertOnePoint correlatedSampler param td (float ts.scale) url elt
                  localCache <- Map.add elt.key newElt localCache
                  newElt
                )
                )
          ({
            points = newPoints;
            keys = ts.keys;
            scale = ts.scale
          }, localCache)
        ) Map.empty
      |> fst
    let gts = {
      triangles = newTriangleSets
      frame = triangleSet.frame
      trianglesByEdge = triangleSet.trianglesByEdge
      trianglesByVertex = triangleSet.trianglesByVertex
    }
    {
      triangleSet = gts;
      geoParams = param;
      tectData = td
    }

  let serialiseTps writer (gps : TectonicParams) = 
    match gps with
    | TP_Default tdp ->
      writer <| sprintf "%.4f\t%b\t%.4f\t%.4f\t%.4f" tdp.heightBiasScale tdp.isVolMultiplicative tdp.volScale tdp.volOfVol tdp.dh_dh_correl
      writer
    | _ -> failwith "nyi"

  let serializeGeoMesh writer (ts : TriangleSet<KeyedPoint<GeoMeshDatum>>) =
    ts.triangles
    writer

  let serialiseGds writer gds =
    let w' = serialiseTps writer gds.geoParams
    let w'' = 
      gds.triangleSet
      |> Array.fold(fun w ts -> serializeGeoMesh w ts)
    1
