module RenderFunctions

open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

open RenderTypes
open GLL

let inBuffer (handle : int) fn bufferType =
  GLL.BindBuffer(bufferType, handle)
  fn(bufferType)
  GLL.BindBuffer(bufferType, 0)

let glRenderType input =
  PrimitiveType.Parse(typedefof<PrimitiveType>, input) :?> PrimitiveType
  
let bindPrimitiveTuple (data : Vector3[]*Vector3[]*int[]*string) = 
  let (vertices, colours, indices,renderType) = data
  let n = Array.length vertices
  let m = Array.length indices
      
  let positionVboHandle = GLL.GenBuffer()
  let normalVboHandle = GLL.GenBuffer()
  let colourVboHandle = GLL.GenBuffer()
  let eboHandle = GLL.GenBuffer()
  
  let bufferPosition bt = 
    GLL.BufferData(bt,
        sizeof<OpenTK.Mathematics.Vector3> * n,
        vertices, BufferUsageHint.StaticDraw)
  
  let bufferColours bt = 
    GLL.BufferData(bt,
        sizeof<OpenTK.Mathematics.Vector3> * n,
        colours, BufferUsageHint.StaticDraw)
  
  let bufferIndices bt = 
    GLL.BufferDataI(bt,
      sizeof<int> * m,
      indices, BufferUsageHint.StaticDraw)
  
  inBuffer positionVboHandle bufferPosition BufferTarget.ArrayBuffer
  inBuffer normalVboHandle bufferPosition BufferTarget.ArrayBuffer
  inBuffer colourVboHandle bufferColours BufferTarget.ArrayBuffer
  inBuffer eboHandle bufferIndices BufferTarget.ElementArrayBuffer
  
  { vertexHandle = positionVboHandle; normalHandle = normalVboHandle; indexHandle = eboHandle; colorHandle = colourVboHandle; numIndices = m; glType = glRenderType renderType }

let bindPrimitiveTuples = List.map bindPrimitiveTuple

let bindPrimitiveData (data : PrimitiveGLData) =
  let n = Array.length data.vertices
  let m = Array.length data.indices
    
  let positionVboHandle = GLL.GenBuffer()
  let normalVboHandle = GLL.GenBuffer()
  let colourVboHandle = GLL.GenBuffer()
  let eboHandle = GLL.GenBuffer()

  let bufferPosition bt = 
    GLL.BufferData(bt,
        sizeof<OpenTK.Mathematics.Vector3> * n,
        data.vertices, BufferUsageHint.StaticDraw)

  let bufferColours bt = 
    GLL.BufferData(bt,
        sizeof<OpenTK.Mathematics.Vector3> * n,
        data.colours, BufferUsageHint.StaticDraw)

  let bufferIndices bt = 
    GLL.BufferDataI(bt,
      sizeof<int> * m,
      data.indices, BufferUsageHint.StaticDraw)

  inBuffer positionVboHandle bufferPosition BufferTarget.ArrayBuffer
  inBuffer normalVboHandle bufferPosition BufferTarget.ArrayBuffer
  inBuffer colourVboHandle bufferColours BufferTarget.ArrayBuffer
  inBuffer eboHandle bufferIndices BufferTarget.ElementArrayBuffer

  { vertexHandle = positionVboHandle; normalHandle = normalVboHandle; indexHandle = eboHandle; colorHandle = colourVboHandle; numIndices = m; glType = glRenderType data.primitiveType }

let unbindPrimitive (gl : BoundPrimitiveGLData) =
  GL.DeleteBuffer gl.vertexHandle
  GL.DeleteBuffer gl.normalHandle
  GL.DeleteBuffer gl.indexHandle
  GL.DeleteBuffer gl.colorHandle
  ()

  
  
let inVaoHandle (vaoHandle : int) fn () =
  GLL.BindVertexArray(vaoHandle)
  fn()
  GLL.BindVertexArray(0)

let copyToVAO (index : int) shaderHandle locationName bt = 
  GLL.EnableVertexAttribArray(index)
  GLL.VertexAttribPointer(index, 3, VertexAttribPointerType.Float, true, Vector3.SizeInBytes, 0)
  GLL.BindAttribLocation(shaderHandle, index, locationName)

let bindIntoVAO (spd : BoundShader) (bhs : BoundPrimitiveGLData) =
  // GL3 allows us to store the vertex layout in a "vertex array object" (VAO).
  // This means we do not have to re-issue VertexAttribPointer calls
  // every time we try to use a different vertex layout - these calls are
  // stored in the VAO so we simply need to bind the correct VAO.
  let vaoHandle = GLL.GenVertexArray()

  let bindInBuffer_ifShaderHasInput shVar glHndl index =
    shVar|> Option.iter (fun label -> inBuffer glHndl (copyToVAO index spd.handle label) BufferTarget.ArrayBuffer)

  let myF () =
    //bindInBuffer_ifShaderHasInput spd.details.positionInput bhs.vertexHandle 0
    //bindInBuffer_ifShaderHasInput spd.details.normalInput bhs.normalHandle 1
    //bindInBuffer_ifShaderHasInput spd.details.colourInput bhs.colorHandle 2
    inBuffer bhs.vertexHandle (copyToVAO 0 spd.handle "in_position") BufferTarget.ArrayBuffer
    inBuffer bhs.normalHandle (copyToVAO 1 spd.handle "in_normal") BufferTarget.ArrayBuffer
    inBuffer bhs.colorHandle (copyToVAO 2 spd.handle "base_colour") BufferTarget.ArrayBuffer
    GLL.BindBuffer(BufferTarget.ElementArrayBuffer, bhs.indexHandle)

  inVaoHandle vaoHandle myF ()
  { prim = bhs; vaoHandle = vaoHandle }

let compileShaders (spd : ShaderProgramData) (defaultUniforms : (string*SomeKindOfUniform) list) (eyeRel, up) =
  let vertexShaderHandle = GLL.CreateShader(ShaderType.VertexShader)
  let fragmentShaderHandle = GLL.CreateShader(ShaderType.FragmentShader)

  GLL.ShaderSource(vertexShaderHandle, spd.vsSource)
  GLL.ShaderSource(fragmentShaderHandle, spd.fsSource)

  GLL.CompileShader(vertexShaderHandle)
  GLL.CompileShader(fragmentShaderHandle)

  let vsError = GL.GetShaderInfoLog(vertexShaderHandle)
  let fsError = GL.GetShaderInfoLog(fragmentShaderHandle)

  let shaderProgramHandle = GL.CreateProgram()

  GLL.AttachShader(shaderProgramHandle, vertexShaderHandle)
  GLL.AttachShader(shaderProgramHandle, fragmentShaderHandle)

  GLL.LinkProgram(shaderProgramHandle)

  let shaderError = GL.GetProgramInfoLog(shaderProgramHandle)

  if vsError <> "" then
    printfn "Xyzzy Vertex Shader Error: %s" vsError
  if fsError <> "" then
    printfn "Xyzzy Fragment Shader Error: %s" fsError
  if shaderError <> "" then
    printfn "Xyzzy Shader Program Error: %s" shaderError

  { details = spd; handle = shaderProgramHandle ; 
    vsError = vsError; fsError = fsError; shaderError = shaderError; 
    defaultUniforms = defaultUniforms
    eyeRel = eyeRel;
    up = up }

let bindShader (cs :CompiledShader) =
  // CG 20230107 - UseProgram needs to be called before the Uniform functions
  GLL.UseProgram(cs.handle)

  let uniforms = 
    cs.defaultUniforms
    |> List.map(fun (unif_name, uf) ->
      let location = GLL.GetUniformLocation(cs.handle, unif_name)
      match uf with
      | UM4(m, _) -> GLL.UniformMatrix4(location, false, ref m)
      | UM4_Stateful(dets) -> GLL.UniformMatrix4(location, false, ref dets.matrix)
      | UV3(v, _) -> GLL.Uniform3(location, v)
      (unif_name, (location, uf)))
    |> Map.ofList


  ({ compiled = cs; handle = cs.handle }, uniforms, cs.eyeRel, cs.up)

let incrementUniform location u = 
  match u with
  | UV3(unif, fOpt) -> 
    match fOpt with
    | Some f -> 
      let unif' = f unif
      GL.Uniform3(location, unif')
      UV3(unif', fOpt)
    | None -> UV3(unif, fOpt)
  | UM4(unif, fOpt) -> 
    match fOpt with
    | Some f -> 
      let unif' = f unif
      GL.UniformMatrix4(location, false, ref unif')
      UM4(unif', fOpt)
    | None -> UM4(unif, fOpt)
  | UM4_Stateful(bk) -> 
    match bk.updaterOpt with
    | Some f -> 
      let (unif', bookKeep') = f bk.bookKeep
      GL.UniformMatrix4(location, false, ref unif')
      let bk' = { matrix = unif'; bookKeep = bookKeep'; updaterOpt = bk.updaterOpt}
      UM4_Stateful(bk')
    | None -> UM4_Stateful(bk)

