namespace FAtlas

open CoordTypes
open TriangleMeshTypes

module TriangleLookupTypes = 
  
  type DirectionKey = char*Cartesian

  type HalfPartition = { upDirection : Cartesian; relevantKey : char; boundary : (char*Cartesian*double) array; normal : Cartesian; threshold : double; normal2 : Cartesian; acosThr : double }
  type Partition = { upper : HalfPartition; lower : HalfPartition }
    
  let deKey (a,_,_) = a
 
  type PartitionKeyResult = { pkrKey : char*char*char; spoke : char; radius : double; angle : double; refDir : char } 
  type PartitionResult<'A> = { pkr : PartitionKeyResult; triangle : SingleTriangle<'A> }

  type CachedTriangleSet<'A> = { uncached : TriangleSet<'A>; partitions : Partition array; mutable lookup : Map<char*char*char, SingleTriangle<'A>>}
    
    

