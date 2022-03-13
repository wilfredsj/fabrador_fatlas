module ElmRender

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input
open OpenTK.Mathematics
open System
open System.Drawing
open OpenTK.Windowing.Desktop

open OpenTK.Windowing.GraphicsLibraryFramework

open RenderTypes
open RenderFunctions
open GLL
open HexaHelpers

let testData2 = hexAsPrimitive [{ row = 0; dicol = 0; z = 0.5f}; { row = 0; dicol = 1; z= 0.8f}; { row = 1; dicol = 0; z= 1.2f}; { row = -1; dicol = 0; z= 1.2f}]

type PassedEvent<'Msg> =
  | KeyDown of char
  | DirectMessage of 'Msg
  | EventNoOp

let direct m = DirectMessage m

type UpdateCallback<'Msg,'Model> = 'Model -> PassedEvent<'Msg> -> 'Model

type ElmLikeWindow<'Msg,'StateModel>(gws, nws, primOveride, updater : UpdateCallback<'Msg,'StateModel>) =
  inherit GameWindow(gws, nws)

  let primOvd : PrimitiveGLData Option = primOveride

  let vertexShaderSource = 
    @"
#version 130
precision highp float;
uniform mat4 projection_matrix;
uniform mat4 modelview_matrix;
in vec3 in_position;
in vec3 in_normal;
in vec3 base_colour;
out vec3 normal;
out vec3 colour2;
void main(void)
{
  //works only for orthogonal modelview
  normal = (modelview_matrix * vec4(in_normal, 0)).xyz;
  colour2 = base_colour;
  
  gl_Position = projection_matrix * modelview_matrix * vec4(in_position, 1);
}"
  let fragmentShaderSource = 
    @"
#version 130
precision highp float;
const vec3 ambient = vec3(0.1, 0.1, 0.1);
const vec3 lightColor = vec3(0.9, 0.9, 0.7);
uniform vec3 lightVecNormalized;
in vec3 normal;
in vec3 colour2;
out vec4 out_frag_color;
void main(void)
{
  float diffuse = clamp(dot(lightVecNormalized, normalize(normal)), 0.0, 1.0);
  //out_frag_color = vec4(ambient + diffuse * lightColor, 1.0);
  out_frag_color = vec4(colour2, 1.0);
}"

  let initModel aspectRatio =
    let spd = makeShader vertexShaderSource fragmentShaderSource
    let sdr = createShaders spd
    
    let modelviewMatrixLocation = GLL.GetUniformLocation(sdr.handle, "modelview_matrix")
    let modelviewMatrix = Matrix4.LookAt(new Vector3(0.0f, 3.0f, 5.0f), new Vector3(0.0f, 0.0f, 0.0f), new Vector3(0.0f, 1.0f, 0.0f))
    GLL.UniformMatrix4(modelviewMatrixLocation, false, ref modelviewMatrix)

    let projectionMatrixLocation = GLL.GetUniformLocation(sdr.handle, "projection_matrix")
    let projectionMatrix = Matrix4.CreatePerspectiveFieldOfView(float32 Math.PI / 4.0f, aspectRatio, 1.0f, 100.0f)
    GLL.UniformMatrix4(projectionMatrixLocation, false, ref projectionMatrix)

    let lightSourceLocation = GLL.GetUniformLocation(sdr.handle, "lightVecNormalized")
    let lightVecNormalized = Vector3(0.5f, -2.5f, 2.0f) |> Vector3.Normalize
    GLL.Uniform3(lightSourceLocation, lightVecNormalized)

    { shader = sdr; 
      primitives = []; 
      modelviewMatrixOpt = Some(modelviewMatrixLocation, modelviewMatrix);
      projectionMatrixOpt = Some(projectionMatrixLocation, projectionMatrix);
      lightSourceOpt = Some(lightSourceLocation, lightVecNormalized);
      vaoHandleOpt = [] }


  let mutable renderModel : RenderModel = initModel 0.75f
  let mutable stateModel : 'StateModel option = None


  let generalCallback = updater
  
  member o.overrideState newState =
    stateModel <- Some newState
    ()

  member o.changeVerticesTuples data = 

    renderModel.primitives |> List.iter unbindPrimitive

    o.VSync <- OpenTK.Windowing.Common.VSyncMode.On

    let glH = bindPrimitiveTuples data
    let vaoH = List.map (bindIntoVAO renderModel.shader) glH
    renderModel <- { renderModel with primitives = glH; vaoHandleOpt = vaoH }

    GLL.Enable(EnableCap.DepthTest)
    GL.ClearColor(System.Drawing.Color.MidnightBlue)
    ()

  member o.changeVertices (data : PrimitiveGLData) =

    renderModel.primitives |> List.iter unbindPrimitive

    o.VSync <- OpenTK.Windowing.Common.VSyncMode.On

    let glH = bindPrimitiveData data
    let vaoH = bindIntoVAO renderModel.shader glH
    renderModel <- { renderModel with primitives = [glH]; vaoHandleOpt = [vaoH] }

    GLL.Enable(EnableCap.DepthTest)
    GL.ClearColor(System.Drawing.Color.MidnightBlue)
    ()
   
        
  override o.OnLoad () =
    o.VSync <- OpenTK.Windowing.Common.VSyncMode.On
    
    let width = o.ClientRectangle.Size.X
    let height = o.ClientRectangle.Size.Y
    let aspectRatio = float32 width / float32 height

    //modelOpt <- Some(initModel aspectRatio)
    
    //primOvd
    //|> Option.iter(o.changeVertices)

    GLL.Enable(EnableCap.DepthTest)
    GL.ClearColor(System.Drawing.Color.MidnightBlue)

  override o.OnUpdateFrame e =
    let newMVM = 
      renderModel.modelviewMatrixOpt
      |> Option.map(fun (modelviewMatrixLocation, modelviewMatrix) -> 
        let rotation = Matrix4.CreateRotationZ(float32 e.Time  |> fun x -> x * -0.25f)
        let rotated = Matrix4.Mult(rotation, modelviewMatrix)
        GL.UniformMatrix4(modelviewMatrixLocation, false, ref rotated)
        (modelviewMatrixLocation, rotated)
      )
    let newLS =
      renderModel.lightSourceOpt
      |> Option.map(fun (lightSourceLoc, lightSource) -> 
        let rotation = Matrix3.CreateRotationX(float32 e.Time |> fun x -> x * 0.0f)
        let rq = Quaternion.FromMatrix(rotation)
        let rotated = Vector3.Transform(lightSource, rq)
        GL.Uniform3(lightSourceLoc, ref rotated)
        (lightSourceLoc, rotated)
      )
    renderModel <- { renderModel with modelviewMatrixOpt = newMVM; lightSourceOpt = newLS }

  override o.OnKeyDown e =
    let charOpt = 
      match e.Key with
      | Keys.A -> Some 'a'
      | Keys.B -> Some 'b'
      | Keys.C -> Some 'c'
      | Keys.D-> Some 'd'
      | Keys.E -> Some 'e'
      | Keys.F -> Some 'f'
      | Keys.G-> Some 'g'
      | Keys.H -> Some 'h'
      | Keys.I -> Some 'i'
      | Keys.J -> Some 'j'
      | Keys.K -> Some 'k'
      | Keys.L -> Some 'l'
      | Keys.M -> Some 'm'
      | Keys.N -> Some 'n'
      | Keys.O -> Some 'o'
      | Keys.P -> Some 'p'
      | Keys.Q -> Some 'q'
      | Keys.R -> Some 'r'
      | Keys.S -> Some 's'
      | Keys.T -> Some 't'
      | Keys.U -> Some 'u'
      | Keys.V -> Some 'v'
      | Keys.W -> Some 'w'
      | Keys.X -> Some 'x'
      | Keys.Y -> Some 'y'
      | Keys.Z -> Some 'z'
      | _ -> None

    let msg = charOpt |> Option.map(KeyDown) |> Option.defaultValue EventNoOp

    stateModel
    |> Option.iter(fun s ->
      let s' = generalCallback s msg
      stateModel <- Some s')
    


  override o.OnRenderFrame e =
    GL.Viewport(0, 0, o.Size.X, o.Size.Y)
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    renderModel.vaoHandleOpt 
    |> List.iter(fun vaoHandle ->
        let gl = vaoHandle.prim
        GL.BindVertexArray(vaoHandle.vaoHandle)
        GL.DrawElements(gl.glType, gl.numIndices,
            DrawElementsType.UnsignedInt, IntPtr.Zero))

    base.SwapBuffers()