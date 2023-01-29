module RenderTypes

open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL

type PrimitiveGLData = { 
  vertices : Vector3 []; 
  colours : Vector3 []; 
  indices : int [];
  primitiveType : string
}  

type BoundPrimitiveGLData = { 
  vertexHandle : int; 
  normalHandle : int; 
  indexHandle : int; 
  colorHandle : int; 
  numIndices : int;
  glType : PrimitiveType
}

type BoundVaoPrimitiveData = {
  prim : BoundPrimitiveGLData
  vaoHandle : int
}

type ShaderProgramData = { 
  fsSource : string; 
  vsSource : string; 
  positionInput : string option; 
  colourInput : string option; 
  normalInput : string option }


type M4BookKeep<'A> = {
  matrix : Matrix4
  bookKeep : 'A
  updaterOpt : ('A -> (Matrix4 *'A)) Option
}

type SomeKindOfUniform =
  | UV3 of (Vector3 * (Vector3 -> Vector3) Option)
  | UM4 of (Matrix4 * (Matrix4 -> Matrix4) Option)
  | UM4_Stateful of M4BookKeep<Matrix4*Matrix4*Matrix4>

let makeShader vss fss =
  { fsSource = fss; 
    vsSource = vss; 
    positionInput = Some("in_position"); 
    normalInput = Some("in_normal"); 
    colourInput = Some("base_colour") }

type CompiledShader = { 
  details : ShaderProgramData; 
  defaultUniforms : (string * SomeKindOfUniform) list;
  handle : int; 
  fsError : string; 
  vsError : string; 
  shaderError : string;
  eyeRel : Vector3;
  up : Vector3}

type BoundShader = { compiled : CompiledShader; handle : int }

type RenderModel = { 
  shader : BoundShader; 
  euclShader : CompiledShader;
  mercShader : CompiledShader;
  primitives : BoundPrimitiveGLData list; 
  uniforms : Map<string, int*SomeKindOfUniform>;  
  vaoHandleOpt : BoundVaoPrimitiveData list;
  eyeVec : Vector3;
  upVec : Vector3
}
