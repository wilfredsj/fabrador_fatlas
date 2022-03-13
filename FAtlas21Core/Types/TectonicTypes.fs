namespace FAtlas

open CoordTypes

open TriangleMeshTypes

module TectonicTypes =
  type BasicPoint = KeyedPoint<Coordinate>

  
  type ClusterData = { 
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
    allClusters : ClusterData[];
    connectionsSoFar : ((int*VertexUrl)*(int*VertexUrl)) list;
    connectedFaces : Set<int*int>
  }

  type CompleteClusterAssignment<'Key when 'Key : comparison> = {
    meshData : TriangleSet<BasicPoint>; 
    clusterAssignments : Map<VertexUrl, int>; 
    lookupFromKey : Map<'Key, int>;
    allClusters : ClusterData[];
    connections : ((int*VertexUrl)*(int*VertexUrl)) list;
    connectedFaces : Set<int*int>
  }
  
  let finishedAssigment u = Array.isEmpty u.unfinishedClusters

  let finalize u = { 
    connections = u.connectionsSoFar ;
    meshData = u.meshData;
    clusterAssignments = u.clusterAssignments ;
    lookupFromKey = u.lookupFromKey ;
    connectedFaces = u.connectedFaces ;
    allClusters = u.allClusters
  }
