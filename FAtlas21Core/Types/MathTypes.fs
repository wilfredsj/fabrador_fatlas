namespace FAtlas

module MathTypes =
  //Am I really doing this?

  type Matrix2x2 = { x : float [] [] }
  
  type SVD_2x2 = {
    evs : float []
    u : Matrix2x2;
    v : Matrix2x2
  }