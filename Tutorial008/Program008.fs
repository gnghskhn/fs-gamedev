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


type RVec3d = {
    x: float
    y: float
    z: float
    }
    with    
    static member plus ((a: RVec3d), (b: RVec3d)) = { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }
    static member scale ((k: float), (a: RVec3d)) = { x = k * a.x; y = k * a.y; z = k * a.z }

let create_RVec3d (x, y, z) =
    { x = x; y = y; z = z; }

type VecOps<'vector, 'scalar> = {
    up: 'vector
    add: ('vector * 'vector) -> 'vector
    scale: ('scalar * 'vector) -> 'vector
}

let inline gravity (vec_ops: VecOps<'a, 'b>) =
    vec_ops.scale (-9.81, vec_ops.up)


let inline spring (vec_ops: VecOps<'a, 'b>) stiff pos =
    vec_ops.scale (-stiff, pos)

     
let inline drag (vec_ops: VecOps<'a, 'b>) k speed =
    vec_ops.scale (-k, speed)

    
let inline euler_implicit (vec_ops: VecOps<'a, 'b>) accel_func pos speed delta =
    let speed2 = vec_ops.add (speed, (vec_ops.scale (delta, (accel_func pos speed))))
    vec_ops.add (pos, (vec_ops.scale (delta, speed2))), speed2


let adapt (f: 'num -> 'num -> 'num) pos speed = 
    let accel = f pos speed
    speed, accel


let inline runge_kutta_4 (vec_ops: VecOps<'a, 'b>) deriv_func pos speed delta =    
    let extrapolate x delta dx = 
        vec_ops.add (x, (vec_ops.scale (delta, dx)))

    let half_delta = 0.5 * delta
         
    let s1, a1 = deriv_func pos speed
    let s2, a2 = deriv_func (extrapolate pos half_delta s1) (extrapolate speed half_delta a1)
    let s3, a3 = deriv_func (extrapolate pos half_delta s2) (extrapolate speed half_delta a2)
    let s4, a4 = deriv_func (extrapolate pos delta s3)      (extrapolate speed delta a3)

    let sixth_delta = delta / 6.0    

    let update old v1 v2 v3 v4 =
        let (*) k v = vec_ops.scale(k, v)
        let (+) u v = vec_ops.add(u, v)
        old + sixth_delta * (v1 + 2.0 * (v2 + v3) + v4)
                             
    let pos1 = update pos s1 s2 s3 s4
    let speed1 = update speed a1 a2 a3 a4
    pos1, speed1

    
let simulate intg_func t0 t_fin delta pos0 speed0 =
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let pos1, speed1 = intg_func pos0 speed0 delta
            repeat t1 pos1 speed1
    repeat t0 pos0 speed0


let run euler_func s0 =
    let t0 = 0.0
    let t_fin = 1000.0
    let delta = 0.005

    let run_timed map_func = 
        let n_runs = 3
        let times =
            [
                for i in 1..n_runs ->
                    let (run_time, res) = 
                        (fun () -> 
                            s0 |> map_func(fun (x, y) -> simulate euler_func t0 t_fin delta x y)
                         ) |> run_time_func
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
    
    
let Vec1D_ops = { up = Vec1d(1.0); add = Vec1d.plus; scale = Vec1d.scale }
let Vec2D_ops = { up = Vec2d(0.0, 1.0); add = Vec2d.plus; scale = Vec2d.scale }
let Vec3D_ops = { up = Vec3d(0.0, 1.0, 0.0); add = Vec3d.plus; scale = Vec3d.scale }
let RVec3D_ops = { up = { x = 0.0; y = 1.0; z = 0.0 }; add = RVec3d.plus; scale = RVec3d.scale }

let inline accel (vec_ops: VecOps<'a, 'b>) (pos: 'a) (speed: 'a) =
    let (+) u v = vec_ops.add(u, v)
    (spring vec_ops 100.0 pos) + (drag vec_ops 1.0 speed) + (gravity vec_ops)

let n_bodies = 100

let accel1D x y = accel Vec1D_ops x y
let accel1D' x y = adapt accel1D x y
let s0_Vec1d = [ for s in 1..n_bodies -> (Vec1d(float s), Vec1d(0.0)) ]
let ef_Vec1d = euler_implicit Vec1D_ops (accel1D)
let rk4_Vec1d = runge_kutta_4 Vec1D_ops (accel1D')

let accel2D x y = accel Vec2D_ops x y
let accel2D' x y = adapt accel2D x y
let s0_Vec2d = [ for s in 1..n_bodies -> (Vec2d(1.0, float s), Vec2d(0.0, 0.0)) ]
let ef_Vec2d = euler_implicit Vec2D_ops (accel2D)
let rk4_Vec2d = runge_kutta_4 Vec2D_ops (accel2D')

let accel3D x y = accel Vec3D_ops x y
let accel3D' x y = adapt accel3D x y
let s0_Vec3d = [ for s in 1..n_bodies -> (Vec3d(1.0, float s, -1.0), Vec3d(0.0, 0.0, 0.0)) ]
let ef_Vec3d = euler_implicit Vec3D_ops (accel3D)
let rk4_Vec3d = runge_kutta_4 Vec3D_ops (accel3D')

let accelR3D x y = accel RVec3D_ops x y
let accelR3D' x y = adapt accelR3D x y
let s0_RVec3d = [ for s in 1..n_bodies -> (create_RVec3d(float s, 1.0, -1.0), create_RVec3d(0.0, 0.0, 0.0)) ]
let ef_RVec3d = euler_implicit RVec3D_ops (accelR3D)
let rk4_RVec3d = runge_kutta_4 RVec3D_ops (accelR3D')

printfn "1D (struct)"
run ef_Vec1d s0_Vec1d
run rk4_Vec1d s0_Vec1d

printfn "2D (struct)"
run ef_Vec2d s0_Vec2d
run rk4_Vec2d s0_Vec2d

printfn "3D (struct)"
run ef_Vec3d s0_Vec3d
run rk4_Vec3d s0_Vec3d

printfn "3D (record)"
run ef_RVec3d s0_RVec3d
run rk4_RVec3d s0_RVec3d

ignore(Console.ReadKey(true))
