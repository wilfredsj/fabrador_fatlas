** NOTES **

AETHER-9:  parameterise boundary of cluster (able to determine (r,th') coordinates

What we have:
- List of Vertex-Pairs that straddle clusters
- O(1) ?? ish function to get neighbours of VertexUrl

Suggested approach:
- Loop through cross-cluster pairs to build Set[Border] per each cluster
- For each cluster
-      Convert set to list
-           precompute for each elt, (r'th') for each
-      Loop through the list O(1) ?? call to get neighbourds, check intersect with Set[]...
-           How many is the maximum? easily 5
-           How many is the minumum... 1
-                What does that mean for the boundary
-                Should we infact be computing (r,th') for the midpoints ...
-                    If we take midpoints, how to be sure of the adjacency
-           for the intersecting neights, pick clockwise

-      Convert set to list
-         Get all neighbours ( O(1)?? call)
-         Sort neighbours clockwise 
-             wrt previous point   <- List.Fold
-             next point in other cluster ? 
-                 yes -> midpoint is new point on the ORDERED BOUNDARY 
-                 no  -> so it's in this cluster -> PASS execution to it
-             few edge cases
-             - stop when we approach ?first? vertex from originally chosen source direction
-             - Hopefully non-issue... we can approach same point two or three times. Should be OK.




