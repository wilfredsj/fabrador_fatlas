namespace FAtlas

open CoordTypes

open TriangleMeshTypes

module TectonicTypes =
  type BasicPoint = KeyedPoint<Coordinate>

  
  type IncompleteClusterDatum = { 
    id : int; 
    members : VertexUrl list; 
    borderValencyOne : Set<VertexUrl>;
    borderValencyTwo : Set<VertexUrl>;
    borderValencyThree : Set<VertexUrl>
  }

  let finishedCluster c = Set.isEmpty c.borderValencyOne && Set.isEmpty c.borderValencyTwo && Set.isEmpty c.borderValencyThree
  
  type ClusterAssigmentState<'Key when 'Key : comparison> = { 
    meshData : TriangleSet<BasicPoint>; 
    clusterAssignments : Map<VertexUrl, int>; 
    lookupFromKey : Map<'Key, int>;
    unfinishedClusters : int [];
    allClusters : IncompleteClusterDatum[];
    connectionsSoFar : ((int*VertexUrl)*(int*VertexUrl)) list;
    connectedFaces : Set<int*int>
  }
  
  // This is a directed vector along the boundary
  //     from inUrl
  //       to outUrl
  //     pt is the cartesian location of the midpoint.
  type ClusterBoundaryPoint = { 
    pt : Cartesian; 
    inUrl : VertexUrl; 
    outUrl : VertexUrl; 
    radius : float; 
    argument : float 
  }
  let bpStr bp = sprintf "In: %s | Out: %s R=%f Th=%f" (vtxStr bp.inUrl) (vtxStr bp.outUrl) bp.radius bp.argument

  type NormalizedBoundarySection = {
    x_lower : float
    x_upper : float
    y_lower : float
    y_upper : float
    dy_dx : float
  }
  let makeNBS xl xu yl yu =
    let dx = xu - xl
    let dy = yu - yl
    { x_lower = xl; x_upper = xu; y_lower = yl; y_upper = yu; dy_dx = dy / dx}
    
  let makeNBS_LR l r =
    if l.argument > r.argument then
      failwith <| sprintf "Bad normalization, left=%A right=%A" l r
    else
      makeNBS l.argument r.argument l.radius r.radius

  type ClusterBoundary = { 
    ref : Cartesian; 
    hub : Cartesian; 
    pts : ClusterBoundaryPoint list 
    normalizedBoundary : NormalizedBoundarySection array
  }

  type CompleteClusterDatum = {
    id : int;
    members : VertexUrl list
    orderedBorder : ClusterBoundary
  }

  type CompleteClusterAssignment<'Key when 'Key : comparison> = {
    meshData : TriangleSet<BasicPoint>; 
    clusterAssignments : Map<VertexUrl, int>; 
    lookupFromKey : Map<'Key, int>;
    allClusters : CompleteClusterDatum[];
    connections : ((int*VertexUrl)*(int*VertexUrl)) list;
    connectedFaces : Set<int*int>
  }

  type ClusterDataForRendering<'Key when 'Key : comparison> = { 
    meshData : TriangleSet<BasicPoint>; 
    membership : Map<'Key, int>; 
    numClusters : int
  }

  let renderCAS (cas : ClusterAssigmentState<'A>)= {
    meshData = cas.meshData; membership = cas.lookupFromKey; numClusters = cas.unfinishedClusters |> Array.length
  }
  let renderCCS (cas : CompleteClusterAssignment<'A>)= {
    meshData = cas.meshData; membership = cas.lookupFromKey; numClusters = cas.allClusters |> Array.length
  }
  
  let finishedAssigment u = Array.isEmpty u.unfinishedClusters
  
  type StressFunction =
  | SmoothLinear
  | PeakAtMid
  | PeakAtSide of float

  type GeoCluster = {
    cluster : CompleteClusterDatum
    heightBias : float
    velocityMagnitude : float
    velocityBearing : float
    stressNeighboursSorted : list<float*(float*StressFunction)>
  }

