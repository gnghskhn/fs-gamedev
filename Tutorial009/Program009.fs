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

type sa<'num> = struct
    val speed:'num
    val accel:'num
    new(speed, accel) = { speed = speed; accel = accel; }
end
    
type Vec1d = struct
    val x: float
    
    new (x) = { x = x }
    
    static member plus ((a: Vec1d), (b: Vec1d)) = Vec1d(a.x + b.x)
    static member scale ((k: float), (a: Vec1d)) = Vec1d(k * a.x)
end


type Vec2d = struct
    val x: float
    val y: float
    
    new (x, y) = { x = x; y = y }
    
    static member plus ((a: Vec2d), (b: Vec2d)) = Vec2d(a.x + b.x, a.y + b.y)
    static member scale ((k: float), (a: Vec2d)) = Vec2d(k * a.x, k * a.y)
end


type Vec3d = struct
    val x: float
    val y: float
    val z: float
    
    new (x, y, z) = { x = x; y = y; z = z }
    
    static member plus ((a: Vec3d), (b: Vec3d)) = Vec3d(a.x + b.x, a.y + b.y, a.z + b.z)
    static member scale ((k: float), (a: Vec3d)) = Vec3d(k * a.x, k * a.y, k * a.z)
end


type CVec1d (x: float) = class
    member v.X = x

    static member plus ((a: CVec1d), (b: CVec1d)) = CVec1d(a.X + b.X)
    static member scale ((k: float), (a: CVec1d)) = CVec1d(k * a.X)
end


type CVec2d (x: float, y:float) = class
    member v.X = x
    member v.Y = y

    static member plus ((a: CVec2d), (b: CVec2d)) = CVec2d(a.X + b.X, a.Y + b.Y)
    static member scale ((k: float), (a: CVec2d)) = CVec2d(k * a.X, k * a.Y)
end


type CVec3d (x: float, y: float, z: float) = class
    member v.X = x
    member v.Y = y
    member v.Z = z
    
    static member plus ((a: CVec3d), (b: CVec3d)) = CVec3d(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member scale ((k: float), (a: CVec3d)) = CVec3d(k * a.X, k * a.Y, k * a.Z)
end

    
let gravity = 
    -9.81


let inline spring k (pos: ^vec) =
    let (*) k x = (^vec: (static member scale: float * ^vec -> ^vec) k, x)
    -k * pos


let inline drag k (pos: ^pos) (speed: ^pos) =
    let (*) k x = (^vec: (static member scale: float * ^vec -> ^vec) k, x)
    -k * speed


let inline euler_implicit accel_func (pos: ^vec) (speed: ^vec) delta =
    let (+) x y = (^vec: (static member plus: ^vec * ^vec -> ^vec) x, y)
    let (*) k x = (^vec: (static member scale: float * ^vec -> ^vec) k, x)
    
    let speed2 = speed + (delta * (accel_func pos speed))    
    let pos2 = pos + (delta * speed2)        
    ps<_>(pos2, speed2)


let adapt (f: 'num -> 'num -> 'num) pos speed = 
    let accel = f pos speed
    sa<_>(speed, accel)


let inline runge_kutta_4 deriv_func (pos: ^vec) (speed:  ^vec) delta =
    let add x y = (^vec: (static member plus: ^vec * ^vec -> ^vec) x, y)
    let scale k x = (^vec: (static member scale: float * ^vec -> ^vec) k, x)
    
    let extrapolate x delta dx = 
        add x (scale delta dx)

    let half_delta = 0.5 * delta
         
    let sa1: sa<_> = deriv_func pos speed
    let sa2 = deriv_func (extrapolate pos half_delta sa1.speed) (extrapolate speed half_delta sa1.accel)
    let sa3 = deriv_func (extrapolate pos half_delta sa2.speed) (extrapolate speed half_delta sa2.accel)
    let sa4 = deriv_func (extrapolate pos delta sa3.speed)      (extrapolate speed delta sa3.accel)

    let sixth_delta = delta / 6.0    

    let update old v1 v2 v3 v4 =
        let (*) k v = scale k v
        let (+) u v = add u v
        old + sixth_delta * (v1 + 2.0 * (v2 + v3) + v4)
                             
    let pos1 = update pos sa1.speed sa2.speed sa3.speed sa4.speed
    let speed1 = update speed sa1.accel sa2.accel sa3.accel sa4.accel
    ps<_>(pos1, speed1)


let simulate intg_func t0 t_fin delta pos0 speed0 =
    let oc = OptimizedClosures.FastFunc3<_,_,_,_>.Adapt(intg_func)
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let ps: ps<_> = oc.Invoke(pos0, speed0, delta)
            repeat t1 ps.pos ps.speed
    repeat t0 pos0 speed0
    

let inline accel (up: ^vec) (pos: ^vec) (speed: ^vec) =
    let (+) x y = (^vec: (static member plus: ^vec * ^vec -> ^vec) x, y)
    let (*) k x = (^vec: (static member scale: float * ^vec -> ^vec) k, x)
    (drag 1.0 pos speed) + (spring 100.0 pos) + (gravity * up)


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

let accel_Vec1d pos speed = accel (Vec1d(1.0)) pos speed
let accel_Vec1d' pos speed = (adapt accel_Vec1d) pos speed
let s0_Vec1d = [ for s in 1..n_bodies -> (Vec1d(float s), Vec1d(0.0)) ]
let ef_Vec1d = euler_implicit (accel_Vec1d)
let rk4_Vec1d = runge_kutta_4 (accel_Vec1d')
printfn "1D (struct)"
run ef_Vec1d s0_Vec1d
run rk4_Vec1d s0_Vec1d

let accel_Vec2d pos speed = accel (Vec2d(0.0, 1.0)) pos speed
let accel_Vec2d' pos speed = (adapt accel_Vec2d) pos speed
let s0_Vec2d = [ for s in 1..n_bodies -> (Vec2d(1.0, float s), Vec2d(0.0, 0.0)) ]
let ef_Vec2d = euler_implicit (accel_Vec2d)
let rk4_Vec2d = runge_kutta_4 (accel_Vec2d')
printfn "2D (struct)"
run ef_Vec2d s0_Vec2d
run rk4_Vec2d s0_Vec2d

let accel_Vec3d pos speed = accel (Vec3d(0.0, 1.0, 0.0)) pos speed
let accel_Vec3d' pos speed = (adapt accel_Vec3d) pos speed
let s0_Vec3d = [ for s in 1..n_bodies -> (Vec3d(1.0, float s, -1.0), Vec3d(0.0, 0.0, 0.0)) ]
let ef_Vec3d = euler_implicit (accel_Vec3d)
let rk4_Vec3d = runge_kutta_4 (accel_Vec3d')
printfn "3D (struct)"
run ef_Vec3d s0_Vec3d
run rk4_Vec3d s0_Vec3d

(*
let s0_CVec1d = [ for s in 1..n_bodies -> (CVec1d(float s), CVec1d(0.0)) ]
let ef_CVec1d = euler_implicit (accel (CVec1d(1.0)))
printfn "1D (class)"
run ef_CVec1d s0_CVec1d

let s0_CVec2d = [ for s in 1..n_bodies -> (CVec2d(1.0, float s), CVec2d(0.0, 0.0)) ]
let ef_CVec2d = euler_implicit (accel (CVec2d(0.0, 1.0)))
printfn "2D (class)"
run ef_CVec2d s0_CVec2d

let s0_CVec3d = [ for s in 1..n_bodies -> (CVec3d(1.0, float s, -1.0), CVec3d(0.0, 0.0, 0.0)) ]
let ef_CVec3d = euler_implicit (accel (CVec3d(0.0, 1.0, 0.0)))
printfn "3D (class)"
run ef_CVec3d s0_CVec3d
*)

ignore (Console.ReadKey(true))
