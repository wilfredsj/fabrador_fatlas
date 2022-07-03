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

type RotationAxis = 
  | FRAx_X of bool
  | FRAx_Y of bool
  | FRAx_Z of bool
  | FRAx_Stop

let testData2 = hexAsPrimitive [{ row = 0; dicol = 0; z = 0.5f}; { row = 0; dicol = 1; z= 0.8f}; { row = 1; dicol = 0; z= 1.2f}; { row = -1; dicol = 0; z= 1.2f}]

type PassedEvent<'Msg> =
  | KeyDown of char
  | KeyEnter
  | DirectMessage of 'Msg
  | EventNoOp

let direct m = DirectMessage m

type UpdateCallback<'Msg,'Model> = 'Model -> PassedEvent<'Msg> -> 'Model

type ElmLikeWindow<'Msg,'StateModel>(gws, nws, primOveride, updater : UpdateCallback<'Msg,'StateModel>) =
  inherit GameWindow(gws, nws)

  let primOvd : PrimitiveGLData Option = primOveride

  // This is probably not viable
  // a triangle in R^3 could be 2-3 triangles when 
  let vertexShaderSource_MercatorProjection = 
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
  
  vec4 fromSphere(in vec4 cart)
  {
      vec4 spherical;
  
      spherical.x = atan(cart.x, cart.y) / 2.5;
      float xy = sqrt(cart.x * cart.x + cart.y * cart.y);
  
      spherical.y = -1.1 + atan(xy, cart.z) / 1.45;
      spherical.z = -1.0 + (spherical.x * spherical.x) * 0.05; 
      spherical.w = cart.w;

      return spherical;
  }
  void main(void)
  {
    normal = vec3(0,0,1);
    normal = (modelview_matrix * vec4(in_normal, 0)).xyz;
    colour2 = base_colour;
    //gl_Position = projection_matrix * modelview_matrix * vec4(fromSphere(in_position), 1);
    gl_Position = fromSphere(modelview_matrix * vec4(in_position, 1));
    //gl_Position = vec4(fromSphere(in_position), 1);
  }"

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
  let rotateProjection dz = fun (modelviewMatrix : Matrix4) ->     
    let rotation = Matrix4.CreateRotationZ(dz)
    Matrix4.Mult(rotation, modelviewMatrix)

  let initModel aspectRatio =
    let mercatorShader = makeShader vertexShaderSource_MercatorProjection fragmentShaderSource
    let euclideanShader = makeShader vertexShaderSource fragmentShaderSource


    let commonUniforms = 
      [
        ("projection_matrix", 
          UM4 (
            Matrix4.CreatePerspectiveFieldOfView(float32 Math.PI / 4.0f, aspectRatio, 0.001f, 100.0f),
            None))
            //None))
        ("lightVecNormalized",
          UV3 (
             Vector3.Normalize <| Vector3(0.5f, -2.5f, 2.0f),
             None
            ))
      ]
    let mercatorUniforms = 
      ("modelview_matrix", UM4 (
        Matrix4.LookAt(new Vector3(0.0f, 0.0f, 0.2f), new Vector3(0.0f, 0.0f, 0.0f), new Vector3(0.0f, 1.0f, 0.0f)), 
        Some <| rotateProjection -0.005f))
       :: commonUniforms
    let euclideanUniforms = 
      ("modelview_matrix", UM4 (
        Matrix4.LookAt(new Vector3(0.0f, 2.0f, 3.0f), new Vector3(0.0f, 0.0f, 0.0f), new Vector3(0.0f, 1.0f, 0.0f)), 
          Some <| rotateProjection -0.005f))
      :: commonUniforms
    let msC = compileShaders mercatorShader mercatorUniforms
    let esC = compileShaders euclideanShader euclideanUniforms
    
    let (shader, uniforms) =  bindShader msC
    { shader = shader;
      euclShader = esC;
      mercShader = msC;
      primitives = []; 
      uniforms = uniforms;
      vaoHandleOpt = [] }


  let mutable renderModel : RenderModel = initModel 0.75f
  let mutable stateModel : 'StateModel option = None


  let generalCallback = updater
  
  member o.overrideState newState =
    stateModel <- Some newState
    ()

  member o.forceEuclidean () =    
    let (shader, uniforms) =  bindShader renderModel.euclShader
    renderModel <- { renderModel with shader = shader; uniforms = uniforms }
    ()
    
  member o.forceMercator () =
    let (shader, uniforms) =  bindShader renderModel.mercShader
    renderModel <- { renderModel with shader = shader; uniforms = uniforms }
    ()

  member o.changeRotationAxis rax =
    let dTh = 0.005f
    let rotation = 
      match rax with
      | FRAx_X dir -> Matrix4.CreateRotationX(if dir then dTh else -dTh)
      | FRAx_Y dir -> Matrix4.CreateRotationY(if dir then dTh else -dTh)
      | FRAx_Z dir -> Matrix4.CreateRotationZ(if dir then dTh else -dTh)
      | FRAx_Stop-> Matrix4.Identity

    let multOp = fun (matrix : Matrix4) -> Matrix4.Mult(rotation, matrix)
    let target_name = "modelview_matrix"
    let newUnif = 
      renderModel.uniforms
      |> Map.find target_name
      |> fun (i, unif) ->
        match unif with
        | UM4(mx, f) -> 
          (i, UM4(mx, Some(multOp)))
        | _ -> (i, unif)

    let uniforms' = Map.add target_name newUnif renderModel.uniforms
    renderModel <- { renderModel with uniforms = uniforms' }




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
    let newUniforms =
      renderModel.uniforms
      |> Map.map(fun n (loc, uv) -> (loc, incrementUniform loc uv))
    renderModel <- { renderModel with uniforms = newUniforms }

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
      | Keys.D1 -> Some '1'
      | Keys.D2 -> Some '2'
      | Keys.D3 -> Some '3'
      | Keys.D4 -> Some '4'
      | Keys.D5 -> Some '5'
      | Keys.D6 -> Some '6'
      | Keys.D7 -> Some '7'
      | Keys.D8 -> Some '8'
      | Keys.D9 -> Some '9'
      | Keys.D0 -> Some '0'
      | Keys.LeftBracket -> Some '['
      | Keys.RightBracket -> Some ']'
      | _ -> None

    

    let specialCharOpt = 
      match e.Key with
      | Keys.Enter
      | Keys.Escape -> true
      | _ -> false

    let msg = 
      match specialCharOpt with
      | true -> KeyEnter
      | false -> 
        charOpt |> Option.map(KeyDown) |> Option.defaultValue EventNoOp
    
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