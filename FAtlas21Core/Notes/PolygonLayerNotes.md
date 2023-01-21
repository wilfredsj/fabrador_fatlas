

  //  Update 2021-12-04

  //  Below ideas seem impractical. 
  //  The correct approach is likely to use a grid
  //  However the grid approach is intrisically different to below idea (Face -> Face, Edge -> Vertex)

  // Triangle subdivision:
  //     New VERTEX at centroid of each face
  //     New vertex connects to 3 parent vertices AND 3 adjacent new vertices




  //
  //        C
  //
  //  /           \ 
  //        |          B
  //
  //  \           /
  //        A
  //
  
  //                
  //        CCC       BCD                        /// AAA -> Aij where  ij circular from A
  //                                             /// ABC -> iii for i i {a,b,c}
  //                                             /// ABC -> ijk where jk in {a,b,c} and i is opposite side from l (l in {a,b,c} l != j,k)
  //    ACF      ABC      BBB
  //
  //
  //        AAA       ABE
  //
  
  
  //                          
  //            C9          B3C3D3              /// A9 -> A5i2jk where i is adjacent to A, jk are adjacent circular to i
  //                 AB2C5D                     /// A3B3B3 ->  i5 j2 k l where i in {a,b,c} j in {a,b,c} k in {a,b,c} and l is opposite side from j
  //          A2BC5F       AB5C2D               /// i5 j2 k l -> i9                       +4i -2j - k - l
  //    A3C3F3       A3B3C3           B9        /// i5 j2 k l -> i3 j3 k3                 -2i +1j +2k - l
  //          A5BC2F                            /// i5 j2 k l -> i3 j3 l3                 -2i +1j - k +2l
  //                                            /// i5 j2 k l -> j5 i2 k l                -3i +3j
  //            A9       A3B3E3                 /// i5 j2 k l -> i5 k2 j l                    - j + k
  //                                            /// i5 j2 k l -> i5 l2 j k                    - j     + l
  //

  // Looks hard to statically determine adjacent keys of vertex.
  // Probably need to keep track of triangle adjacencies

  // Sheaf = { Triangle List; Adjacency List }


  // Can make Map<OldVertex, NewVertex list> with complete list of triangles

  // IF we Map<OldVertex, NewVertex list>
  // then can define faces for a NewVertex
  //      new Triangles are <OldVertex, Parent_i, OtherChild_OfParent_i>

  //      TriangleAdjacencies are <SameVertex, SameParent,  OtherOtherChild_OfParent_i>
  //                              <SameVertex, OtherCommonParent,  OtherChild_OfParent_i>
  //                              <NextChildOfParent, SameParent, OtherOtherChild_OfParent_i>

  //type LayerBookkeeping = { 
  //  triangles: KeyTriangle[];
  //  faceMembership : Map<CoordinateKeys, KeyTriangle []>; 
  //  edgeNeighbours : Map<CoordinateKeys, CoordinateKeys []>}

