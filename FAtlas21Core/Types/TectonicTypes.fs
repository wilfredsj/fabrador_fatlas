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
  
  type ClusterBoundaryPoint = { pt : Cartesian; inUrl : VertexUrl; outUrl : VertexUrl; radius : float; argument : float }
  let bpStr bp = sprintf "In: %s | Out: %s R=%f Th=%f" (vtxStr bp.inUrl) (vtxStr bp.outUrl) bp.radius bp.argument
  type ClusterBoundary = { ref : Cartesian; hub : Cartesian; pts : ClusterBoundaryPoint list }

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

  type ClusterDataForRendering<'Key when 'Key : comparison>  = { meshData : TriangleSet<BasicPoint>; membership : Map<'Key, int>; numClusters : int}

  let renderCAS (cas : ClusterAssigmentState<'A>)= {
    meshData = cas.meshData; membership = cas.lookupFromKey; numClusters = cas.unfinishedClusters |> Array.length
  }
  let renderCCS (cas : CompleteClusterAssignment<'A>)= {
    meshData = cas.meshData; membership = cas.lookupFromKey; numClusters = cas.allClusters |> Array.length
  }
  
  let finishedAssigment u = Array.isEmpty u.unfinishedClusters