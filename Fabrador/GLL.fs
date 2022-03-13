module GLL


open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open OpenTK.Mathematics

let CreateShader a =
  printfn "CreateShader"
  GL.CreateShader a
  
let ShaderSource (a, b) =
  printfn "ShaderSource"
  GL.ShaderSource(a, b)
  
let CompileShader (a:int) =
  printfn "CompileShader"
  GL.CompileShader a
  
let AttachShader (a:int, b:int) =
  printfn "AttachShader"
  GL.AttachShader(a, b)
  
let LinkProgram (a:int) =
  printfn "LinkProgram"
  GL.LinkProgram a

let UseProgram (a:int) =
  printfn "UseProgram"
  GL.UseProgram a
  
let GetUniformLocation (a:int, b:string) =
  let r = GL.GetUniformLocation(a, b)
  printfn "GetUniformLocation %i %s (%i)" a b r
  r

let UniformMatrix4 (a:int, b:bool, c: Matrix4 ref) =
  printfn "UniformMatrix4 %i" a
  GL.UniformMatrix4(a, b, c)

let Uniform3 (a:int, b:Vector3) =
  printfn "Uniform3 %i" a
  GL.Uniform3(a, b)

let Uniform3r (a:int, b:Vector3 ref) =
  printfn "Uniform3 %i" a
  GL.Uniform3(a, b)

let BindVertexArray (a:int) =
  printfn "BindVertexArray %i" a
  GL.BindVertexArray a

let BindBuffer (t, a:int) =
  GL.BindBuffer (t,a)
  printfn "BindBuffer %A %i" t a
  ()

let GenBuffer () =
  let r = GL.GenBuffer()
  printfn "GenBuffer %i" r
  r

let GenVertexArray () =
  let r = GL.GenVertexArray()
  printfn "GenVertexArray %i" r
  r

let BufferData (t : BufferTarget, n : int, v : Vector3[], h) =
  printfn "BufferData %A %i" t n
  GL.BufferData(t,n,v,h)

let BufferDataI (t : BufferTarget, n : int, v : int[], h) =
  printfn "BufferData %A %i" t n
  GL.BufferData(t,n,v,h)

let EnableVertexAttribArray (a : int) = 
  printfn "EnableVertexAttribArray %i" a
  GL.EnableVertexAttribArray a

let VertexAttribPointer (a:int,b:int,c: VertexAttribPointerType,d:bool,e:int,f:int) =
  printfn "VertexAttribPointer %i size=%i stride=%i offset=%i" a b e f
  GL.VertexAttribPointer (a,b,c,d,e,f)

let BindAttribLocation(a:int, b:int, c:string) =
  printfn "BindAttribLocation %i %i %s" a b c
  GL.BindAttribLocation (a,b,c)

let Enable (a:EnableCap) =
  printfn "Enable %A" a
  GL.Enable a
  