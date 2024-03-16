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
    argument : float 
    radius : float; 
    pt : Cartesian; 
    inUrl : VertexUrl; 
    outUrl : VertexUrl; 
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

  let makeNBS_RL r l =
    if l.argument > r.argument then
      failwith <| sprintf "Bad normalization, left=%A right=%A" l r
    else
      makeNBS l.argument r.argument l.radius r.radius

  type ClusterBoundary = { 
    ref : Cartesian; 
    hub : Cartesian; 
    pts : ClusterBoundaryPoint list 
    normalizedBoundary : (float*(float*NormalizedBoundarySection)) array
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

  
  //
  //
  //      _=^
  //   _=^
  // =^
  //
  //    ^__==^
  //   =  
  // =^

  //
  //   ^   _=^
  //  = =_^
  // =   
  
  type StressFunction =
  | SmoothLinear    
  | PeakAtMid of bool
  | PeakAtSide of float

  type FormGeneratorParams = {
    stressThresholds : float array
    smallDH : float    
    bigDH : float
    pSameDirPeakArr : float array array
    pOppositeDirPeakArr : float array array
    pSideArr : float array array
    pPeakArr : float array array
    pTroughArr : float array array
  }

  type FormStatistic = {
    stress : float
    dh : float
    avgH : float
    formChosen : StressFunction
  }

  type GeoClusterBoundary = {
    thisBearing : float
    oppositeBearing : float
    stress : float
    form : StressFunction
    thisId : int
    oppositeId : int
    thisMidBias : float
    oppositeMidBias : float
  }

  let reverseSf = 
    function
    | SmoothLinear -> SmoothLinear
    | PeakAtMid b -> PeakAtMid b
    | PeakAtSide x -> PeakAtSide -x


  let reverse gcb = {
    oppositeBearing = gcb.thisBearing;
    thisBearing = gcb.oppositeBearing
    stress = gcb.stress
    form = reverseSf gcb.form
    thisId = gcb.oppositeId
    oppositeId = gcb.thisId
    thisMidBias = gcb.oppositeMidBias
    oppositeMidBias = gcb.thisMidBias
  }

  type TectonicCluster = {
    cluster : CompleteClusterDatum
    heightBias : float
    // unitVelocity is a Euclidean targent vector
    unitVelocity : Cartesian
    velocityMagnitude: float
    stressNeighboursSorted : (GeoClusterBoundary*GeoClusterBoundary) array
  }

  type TectonicData<'Key when 'Key : comparison> = {
    cca : CompleteClusterAssignment<'Key>
    plates : TectonicCluster array
  }

  type TectonicDefaultParams = {
    heightBiasScale : float
    isVolMultiplicative : bool
    volScale : float
    volOfVol: float
    dh_dh_correl : float
  }

  type TectonicParams = 
  | TP_Default of TectonicDefaultParams

  let defaultTechParams = 
    TP_Default {
      heightBiasScale = 0.1
      isVolMultiplicative = true
      volScale = 0.1
      volOfVol = 1.6
      dh_dh_correl = 0.8
    }

