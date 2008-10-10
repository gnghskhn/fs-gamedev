#light

open System


let run_time_func (func: unit -> 'a) =
    let watch = new System.Diagnostics.Stopwatch()
    watch.Start()
    let res = func ()
    watch.Stop()
    let diff0 = float watch.ElapsedMilliseconds / 1000.0
    (diff0, res)


let map_parallel func items =
    let tasks =
        seq {
            for i in items -> async {
                return (func i)
            }
        }
    Async.Run (Async.Parallel tasks)

type ps<'num> = struct
    val pos:'num
    val speed:'num
    new(pos, speed) = { pos = pos; speed = speed }
end
    
type VecN =
    One of float
    | Two of float*float
    | Three of float*float*float
with
    static member inline plus (a: VecN) (b: VecN) =
        match (a, b) with
            | (One(v), One(w)) -> One(v+w)
            | (Two(v0, v1), Two(w0, w1)) -> Two(v0 + w0, v1 + w1)
            | (Three(v0, v1, v2), Three(w0, w1, w2)) -> Three(v0 + w0, v1 + w1, v2 + w2)
            | _ -> failwith "Size mismatch"

    static member inline scale (k: float) (a: VecN) =
        match a with
            | One(v) -> One(k * v)
            | Two(v0, v1) -> Two(k * v0, k * v1)
            | Three(v0, v1, v2)  -> Three(k * v0, k * v1, k * v2)


type Vec1d = struct
    val x: float
    
    new (x) = { x = x }
    
    static member inline plus (a: Vec1d) (b: Vec1d) = Vec1d(a.x + b.x)
    static member inline scale (k: float) (a: Vec1d) = Vec1d(k * a.x)
end


type Vec2d = struct
    val x: float
    val y: float
    
    new (x, y) = { x = x; y = y }
    
    static member inline plus (a: Vec2d) (b: Vec2d) = Vec2d(a.x + b.x, a.y + b.y)
    static member inline scale (k: float) (a: Vec2d) = Vec2d(k * a.x, k * a.y)
end


type Vec3d = struct
    val x: float
    val y: float
    val z: float
    
    new (x, y, z) = { x = x; y = y; z = z }
    
    static member inline plus (a: Vec3d) (b: Vec3d) = Vec3d(a.x + b.x, a.y + b.y, a.z + b.z)
    static member inline scale (k: float) (a: Vec3d) = Vec3d(k * a.x, k * a.y, k * a.z)
end


type CVec1d (x: float) = class
    member v.X = x

    static member inline plus (a: CVec1d) (b: CVec1d) = CVec1d(a.X + b.X)
    static member inline scale (k: float) (a: CVec1d) = CVec1d(k * a.X)
end


type CVec2d (x: float, y:float) = class
    member v.X = x
    member v.Y = y

    static member inline plus (a: CVec2d) (b: CVec2d) = CVec2d(a.X + b.X, a.Y + b.Y)
    static member inline scale (k: float) (a: CVec2d) = CVec2d(k * a.X, k * a.Y)
end


type CVec3d (x: float, y: float, z: float) = class
    member v.X = x
    member v.Y = y
    member v.Z = z
    
    static member inline plus (a: CVec3d) (b: CVec3d) = CVec3d(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member inline scale (k: float) (a: CVec3d) = CVec3d(k * a.X, k * a.Y, k * a.Z)
end

    
let gravity = 
    -9.81


let inline spring (pos: ^vec) =
    let (*) k x = (^vec: (static member inline scale: float -> ^vec -> ^vec) k) x
    let k = 100.0
    -k * pos


let inline drag k (pos: ^pos) (speed: ^pos) =
    let (*) k x = (^vec: (static member inline scale: float -> ^vec -> ^vec) k) x
    -k * speed


let inline euler_implicit accel_func (pos: ^vec) (speed: ^vec) delta =
    let (+) x y = (^vec: (static member inline plus: ^vec -> ^vec -> ^vec) x) y
    let (*) k x = (^vec: (static member inline scale: float -> ^vec -> ^vec) k) x
    
    let speed2 = speed + (delta * (accel_func pos speed))    
    let pos2 = pos + (delta * speed2)        
    ps<_>(pos2, speed2)


let inline simulate intg_func t0 t_fin delta pos0 speed0 =
    let oc = OptimizedClosures.FastFunc3<_,_,_,_>.Adapt(intg_func)
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let ps: ps<_> = oc.Invoke(pos0, speed0, delta)
            repeat t1 ps.pos ps.speed
    repeat t0 pos0 speed0
    

let inline accel (up: ^vec) (pos: ^vec) (speed: ^vec) =
    let (+) x y = (^vec: (static member inline plus: ^vec -> ^vec -> ^vec) x) y
    let (*) k x = (^vec: (static member inline scale: float -> ^vec -> ^vec) k) x
    (drag 1.0 pos speed) + (spring pos) + (gravity * up)


let run euler_func s0 =
    let t0 = 0.0
    let t_fin = 1000.0
    let delta = 0.005

    let run_timed map_func = 
        let n_runs = 3
        let times =
            [
                for i in 1..n_runs ->
                    let (run_time, res) = run_time_func (fun () -> s0 |> map_func(fun (x, y) -> simulate euler_func t0 t_fin delta x y))
                    run_time
            ]
        
        let max = Seq.max times
        let min = Seq.min times
        let avg = (Seq.fold (fun x y -> x+y) 0.0 times) / float n_runs
        (min, max, avg)

    let single_min, single_max, single_avg = run_timed List.map
    let multi_min, multi_max, multi_avg = run_timed map_parallel

    printfn "Single (min, max, avg): %f %f %f" single_min single_max single_avg
    printfn "Multi (min, max, avg): %f %f %f" multi_min multi_max multi_avg


let n_bodies = 100

let s0_Vec1d = [ for s in 1..n_bodies -> (Vec1d(float s), Vec1d(0.0)) ]
let ef_Vec1d = euler_implicit (accel (Vec1d(1.0)))

let s0_Vec2d = [ for s in 1..n_bodies -> (Vec2d(1.0, float s), Vec2d(0.0, 0.0)) ]
let ef_Vec2d = euler_implicit (accel (Vec2d(0.0, 1.0)))

let s0_Vec3d = [ for s in 1..n_bodies -> (Vec3d(1.0, float s, -1.0), Vec3d(0.0, 0.0, 0.0)) ]
let ef_Vec3d = euler_implicit (accel (Vec3d(0.0, 1.0, 0.0)))

let s0_CVec1d = [ for s in 1..n_bodies -> (CVec1d(float s), CVec1d(0.0)) ]
let ef_CVec1d = euler_implicit (accel (CVec1d(1.0)))

let s0_CVec2d = [ for s in 1..n_bodies -> (CVec2d(1.0, float s), CVec2d(0.0, 0.0)) ]
let ef_CVec2d = euler_implicit (accel (CVec2d(0.0, 1.0)))

let s0_CVec3d = [ for s in 1..n_bodies -> (CVec3d(1.0, float s, -1.0), CVec3d(0.0, 0.0, 0.0)) ]
let ef_CVec3d = euler_implicit (accel (CVec3d(0.0, 1.0, 0.0)))

(*
let s0_One = [ for s in 1..n_bodies -> (One(float s), One(0.0)) ]
let ef_One = euler_implicit (accel (One(1.0)))

let s0_Two = [ for s in 1..n_bodies -> (Two(float s, 1.0), Two(0.0, 0.0)) ]
let ef_Two = euler_implicit (accel (Two(0.0, 1.0)))

let s0_Three = [ for s in 1..n_bodies -> (Three(float s, 1.0, -1.0), Three(0.0, 0.0, 0.0)) ]
let ef_Three = euler_implicit (accel (Three(0.0, 1.0, 0.0)))
*)
printfn "1D (struct)"
run ef_Vec1d s0_Vec1d

printfn "2D (struct)"
run ef_Vec2d s0_Vec2d
 
printfn "3D (struct)"
run ef_Vec3d s0_Vec3d
(*
printfn "1D (class)"
run ef_CVec1d s0_CVec1d

printfn "2D (class)"
run ef_CVec2d s0_CVec2d
 
printfn "3D (class)"
run ef_CVec3d s0_CVec3d

printfn "1D (union)"
run ef_One s0_One

printfn "2D (union)"
run ef_Two s0_Two
 
printfn "3D (union)"
run ef_Three s0_Three
*) 
ignore (Console.ReadKey(true))
