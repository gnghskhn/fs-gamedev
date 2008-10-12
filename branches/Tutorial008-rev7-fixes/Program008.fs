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
    
    static member plus (a: Vec1d) (b: Vec1d) = Vec1d(a.x + b.x)
    static member scale (k: float) (a: Vec1d) = Vec1d(k * a.x)
end


type Vec2d = struct
    val x: float
    val y: float
    
    new (x, y) = { x = x; y = y }
    
    static member plus (a: Vec2d) (b: Vec2d) = Vec2d(a.x + b.x, a.y + b.y)
    static member scale (k: float) (a: Vec2d) = Vec2d(k * a.x, k * a.y)
end


type Vec3d = struct
    val x: float
    val y: float
    val z: float
    
    new (x, y, z) = { x = x; y = y; z = z }
    
    static member plus (a: Vec3d) (b: Vec3d) = Vec3d(a.x + b.x, a.y + b.y, a.z + b.z)
    static member scale (k: float) (a: Vec3d) = Vec3d(k * a.x, k * a.y, k * a.z)
end


type VecOps<'vector, 'scalar> = class
    val up: 'vector
    val add: 'vector -> 'vector -> 'vector
    val scale: 'scalar -> 'vector -> 'vector
    
    new (up, add, scale) = { up = up; add = add; scale = scale; }
end


let gravity (vec_ops: VecOps<'a, 'b>) =
    vec_ops.scale -9.81 vec_ops.up


let spring (vec_ops: VecOps<'a, 'b>) stiff pos =
    vec_ops.scale -stiff pos

     
let drag (vec_ops: VecOps<'a, 'b>) k speed =
    vec_ops.scale (-k) speed

    
let euler_implicit (vec_ops: VecOps<'a, 'b>) accel_func pos speed delta =
    let speed2 = vec_ops.add speed (vec_ops.scale delta (accel_func pos speed))
    vec_ops.add pos (vec_ops.scale delta speed2), speed2


let adapt (f: 'num -> 'num -> 'num) pos speed = 
    let accel = f pos speed
    speed, accel


let runge_kutta_4 (vec_ops: VecOps<'a, 'b>) deriv_func pos speed delta =
    let extrapolate x delta dx = 
        vec_ops.add x (vec_ops.scale delta dx)

    let half_delta = 0.5 * delta
         
    let s1, a1 = deriv_func pos speed
    let s2, a2 = deriv_func (extrapolate pos half_delta s1) (extrapolate speed half_delta a1)
    let s3, a3 = deriv_func (extrapolate pos half_delta s2) (extrapolate speed half_delta a2)
    let s4, a4 = deriv_func (extrapolate pos delta s3)      (extrapolate speed delta a3)

    let sixth_delta = delta / 6.0    

    let update old v1 v2 v3 v4 =
        vec_ops.add old
            (vec_ops.scale sixth_delta
                (vec_ops.add v1
                (vec_ops.add v2
                (vec_ops.add v2
                (vec_ops.add v3
                (vec_ops.add v3
                             v4))))))
                             
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
    
    
let Vec1D_ops = VecOps<Vec1d, float>(Vec1d(1.0), Vec1d.plus, Vec1d.scale)
let Vec2D_ops = VecOps<Vec2d, float>(Vec2d(0.0, 1.0), Vec2d.plus, Vec2d.scale)
let Vec3D_ops = VecOps<Vec3d, float>(Vec3d(0.0, 1.0, 0.0), Vec3d.plus, Vec3d.scale)

let accel (vec_ops: VecOps<'a, 'b>) (pos: 'a) (speed: 'a) =
    vec_ops.add (spring vec_ops 100.0 pos)
        (vec_ops.add (drag vec_ops 1.0 speed) (gravity vec_ops))

let n_bodies = 100

let s0_Vec1d = [ for s in 1..n_bodies -> (Vec1d(float s), Vec1d(0.0)) ]
let ef_Vec1d = euler_implicit Vec1D_ops (accel Vec1D_ops)
let rk4_Vec1d = runge_kutta_4 Vec1D_ops (adapt (accel Vec1D_ops))

let s0_Vec2d = [ for s in 1..n_bodies -> (Vec2d(float s, 1.0), Vec2d(0.0, 0.0)) ]
let ef_Vec2d = euler_implicit Vec2D_ops (accel Vec2D_ops)
let rk4_Vec2d = runge_kutta_4 Vec2D_ops (adapt (accel Vec2D_ops))

let s0_Vec3d = [ for s in 1..n_bodies -> (Vec3d(float s, 1.0, -1.0), Vec3d(0.0, 0.0, 0.0)) ]
let ef_Vec3d = euler_implicit Vec3D_ops (accel Vec3D_ops)
let rk4_Vec3d = runge_kutta_4 Vec3D_ops (adapt (accel Vec3D_ops))

printfn "1D (struct)"
run ef_Vec1d s0_Vec1d
run rk4_Vec1d s0_Vec1d

printfn "2D (struct)"
run ef_Vec2d s0_Vec2d
run rk4_Vec2d s0_Vec2d

printfn "3D (struct)"
run ef_Vec3d s0_Vec3d
run rk4_Vec3d s0_Vec3d

ignore(Console.ReadKey(true))
