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

type ShaderProgramData = { fsSource : string; vsSource : string; positionInput : string option; colourInput : string option; normalInput : string option }

let makeShader vss fss =
  { fsSource = fss; vsSource = vss; positionInput = Some("in_position"); normalInput = Some("in_normal"); colourInput = Some("base_colour") }

type BoundShader = { details : ShaderProgramData; handle : int; fsError : string; vsError : string; shaderError : string }

type RenderModel = { 
  shader : BoundShader; 
  primitives : BoundPrimitiveGLData list; 
  modelviewMatrixOpt : (int*Matrix4) option;
  projectionMatrixOpt : (int*Matrix4) option;
  lightSourceOpt : (int*Vector3) option;
  vaoHandleOpt : BoundVaoPrimitiveData list
}