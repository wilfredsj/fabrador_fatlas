﻿** TODO / DONE **

  // DONE 
  // Fixed - 1: Why fail when ClusterIterate 50?
  // Fixed - 2: If vertex added to 1 cluster, needs to be removed from border of other clusters
  // Fixed - 3: If x-times in border, auto-add?
  // Fixed - 4: Which clusters touch other clusters?
  // Fixed - 5: Terminal condition ?
  // Fixed - 6: Keyboard callback to Atlas
  // Fixed - 7: Decouple Render modes
  // Fixed -14: Plot mode based on mercator GRID 
  // Fixed -19: Option to render cluster without wireframe 
  // Fixed -20: Able to Init/Divide/ClusterIterate from i/o
  // Fixed -21: Runtime control over RNG seed
  // Fixed -22: Remove duplication in "interior from edge" logic
  // Fixed -23: Test cases for all neighbours being normalized (n=4 and n=3)
  // Fixed -24: Test case for all points being reasonably adjacent
  //	   -27: Remove dependency of Fabrador on FAtlas
  //       -26: Separate out parts of AtlasViewFunctions that does not depend on AtlasStateTypes
  //       -18: 2D plot based on Mercator *Projection*
  //       -25: Implement some shader for 2D projection
  //       -28: Change rotation directions
  //       -30: Reverse/Stop rotation direction
  //       -15: Unparameterised boundary
  //       -9:  Parameterize boundary of cluster (able to determine (r,th') coordinates
  //       -16: Render mode to see raw boundary
  //       -31: Find out why boundary of idx=8/idx=18 not correct in default RNG seed (1138)
  //       -17: Render mode to see (r',th') from parameterised boundary *on the boundary*
  //       -32: Render mode to see (r',th') from parameterised boundary *on the icosagrid*
  //       -33: Same as -32 but apparently done again??
  //       -34: Seems non-mercator projection is broken (GL function call ordering)
  //       -8:  Assign base heights to clusters
  //       -10: Assign momentum to cluster
  //       -11: Stress function based on momentum
  //       -12: Render mode for stress
  //       -36: Stress should be based on dot of dV with dX
  //       -13: Render mode for base height
  //       -41: Render mode using actual 'r' coordinate from e.g. flat height bias
  //       -37: Render mode for flat interpolated (zero-stress) height
  //       -38: Implement API to get stressed height for from the isogrid
  //       -39: Render mode for stressed height
  //       -42: Render mode using colour for contours
  //       -43: Think about how to sample from height bias -> height.
  //       -44: Implement division on GeoMesh
  //       -48: Geo render mode with stress
  //       -49: Fix upsample / downsample issue with GeoMeshView
  //       -50: Render mode with colours floored at 'sea' level
  //       -51: Render mode with radius floored at 'sea' level
  //       -40: Remove mystery Geo_ implementation file
  //       -47: Do we need SphereMeshTypes / SphereMeshFunctions files at all? Can move the comments to Notes.md
  //       -29: Solve gimbal lock in at least euclidean case
  //       -58: Avoid DRY of Eye location / up in Euclidean gimbal-lock-avoid
  //       -59: Solve gimbal lock in Mercator case
  //       -57: Can IsoGrid make Hex/Pent-like display
  //       -60: Fix Hex/Pent-like display tiling issues
  //       -61: Fix Hex/Pent-like display logic issues - same logical face should have same colour/z from any triangle (issue with initial conversion from IsoGrid)
  //       -46: Change sampleHeightVol to use joint normal, at least in some settings
  //       -56: Console I/O version  
  //       -62: Console histograms of e.g. heightbias     
  //       -65: Console Triangle plot
  //       -66: Console Triangle plot has colours
  //       -67: Console Triangle plot has legend
  //       -68: Fix legend padding in triangle plot
  //       -70: Histogram of number of elements in each cluster (i.e. continent size)
  //       -72: Inverse function for urlToKey
  //       -71: Console Triangle plot has cluster/continent membership
  //       -73: Console Triangle plot can exclude non-canonical vertices
  //       -74: Add some cache of recent params to console
  // 	   -69: Console Triangle plot has floor value (e.g. sea level) 
  //       -76: Support console input in UI app
  //	   -79: Tidy up println spam
  //       -78: Console can list valid commands
  //       -80: Some way to make console stay in console mode
  //       -82: Re-invent curses-like display for layering
  //       -84: Can't turn off sea level in console triangle plot
  //       -83: Migrate rest of triangle plots to Console Grid (aka Curses-like display)
  //       -81: Show indices of neighbouring triangles in triangle plot
  //       -85: Add a layer with scale-related information
  //       -86: Add ability to query cluster velocity and stress profile from console (debug t=6 on seed 10101) (Note - is interaction between cluster ID 'M' and 'G' with PeakAtSide stress function)
  //       -89: Something strange with 'radius' interpolation on seed=10101 + { x = 0.5633320074, y = 0.2323790856, z = 0.6209747922 } clusterid=6
  //       -90: Investigate double-spike in heightbias between 6, M on seed=10101
  //       -91: -87 and -88 need to go through mid of common border, not mid of the two centroids  (e.g. plot tect fc=2 tc=21 on seed 10101)
  //       -87: Plot heightbias as function between centroids of two clusters (e.g. M / G on seed=10101)



  // TODO:
  // AETHER
  //       -35: Some kind of legend for colour scale
  //       -45: Work out how to test / visualize hardcoded parameters
  //       -52: Id Land masses
  //       -53: Id sea bodies
  //       -54: ID drainage basins
  //       -55: GeoMesh <-> file
  //       -63: Console histograms of geomesh feature
  //       -64: Console histograms of land mass / sea bodies
  //       -75: Display multiple triangle plots at once
  //       -88: Plot actual height as function between centroids of two clusters (e.g. M / G on seed=10101)


  
  //  NOT DOING:
  //       -77: Console is flipped relative to UI, for t=7. Is it in all cases? Not certain this is important