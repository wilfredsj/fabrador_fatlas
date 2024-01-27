module GLL


open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open OpenTK.Mathematics

let writingToConsole = false

let CreateShader a =
  if writingToConsole then printfn "CreateShader" else ()
  GL.CreateShader a
  
let ShaderSource (a, b) =
  if writingToConsole then printfn "ShaderSource" else ()
  GL.ShaderSource(a, b)
  
let CompileShader (a:int) =
  if writingToConsole then printfn "CompileShader" else ()
  GL.CompileShader a
  
let AttachShader (a:int, b:int) =
  if writingToConsole then printfn "AttachShader" else ()
  GL.AttachShader(a, b)
  
let LinkProgram (a:int) =
  if writingToConsole then printfn "LinkProgram" else ()
  GL.LinkProgram a

let UseProgram (a:int) =
  if writingToConsole then printfn "UseProgram" else ()
  GL.UseProgram a
  
let GetUniformLocation (a:int, b:string) =
  let r = GL.GetUniformLocation(a, b)
  if writingToConsole then printfn "GetUniformLocation %i %s (%i)" a b r else ()
  r

let UniformMatrix4 (a:int, b:bool, c: Matrix4 ref) =
  if writingToConsole then printfn "UniformMatrix4 %i" a else ()
  GL.UniformMatrix4(a, b, c)

let Uniform3 (a:int, b:Vector3) =
  if writingToConsole then printfn "Uniform3 %i" a else ()
  GL.Uniform3(a, b)

let Uniform3r (a:int, b:Vector3 ref) =
  if writingToConsole then printfn "Uniform3 %i" a else ()
  GL.Uniform3(a, b)

let BindVertexArray (a:int) =
  if writingToConsole then printfn "BindVertexArray %i" a else ()
  GL.BindVertexArray a

let BindBuffer (t, a:int) =
  GL.BindBuffer (t,a)
  if writingToConsole then printfn "BindBuffer %A %i" t a else ()
  ()

let GenBuffer () =
  let r = GL.GenBuffer()
  if writingToConsole then printfn "GenBuffer %i" r else ()
  r

let GenVertexArray () =
  let r = GL.GenVertexArray()
  if writingToConsole then printfn "GenVertexArray %i" r else ()
  r

let BufferData (t : BufferTarget, n : int, v : Vector3[], h) =
  if writingToConsole then printfn "BufferData %A %i" t n else ()
  GL.BufferData(t,n,v,h)

let BufferDataI (t : BufferTarget, n : int, v : int[], h) =
  if writingToConsole then printfn "BufferData %A %i" t n else ()
  GL.BufferData(t,n,v,h)

let EnableVertexAttribArray (a : int) = 
  if writingToConsole then printfn "EnableVertexAttribArray %i" a else ()
  GL.EnableVertexAttribArray a

let VertexAttribPointer (a:int,b:int,c: VertexAttribPointerType,d:bool,e:int,f:int) =
  if writingToConsole then printfn "VertexAttribPointer %i size=%i stride=%i offset=%i" a b e f else ()
  GL.VertexAttribPointer (a,b,c,d,e,f)

let BindAttribLocation(a:int, b:int, c:string) =
  if writingToConsole then printfn "BindAttribLocation %i %i %s" a b c else ()
  GL.BindAttribLocation (a,b,c)

let Enable (a:EnableCap) =
  if writingToConsole then printfn "Enable %A" a else ()
  GL.Enable a
  