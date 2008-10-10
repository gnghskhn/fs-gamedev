#light

open System

let run_time_func (func: unit -> 'a) =
    let time0 = System.DateTime.Now
    let res = func ()
    let diff0 = System.DateTime.Now - time0
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
    new(speed, accel) = { speed = speed; accel = accel }
end
        
let gravity = 
    -9.81

let inline spring pos =
    let k = 100.0 in
        -k * pos

let inline drag k pos speed = -k * speed
    
let inline euler_implicit accel_func pos speed delta =
    let speed2 = speed + delta * accel_func pos speed in
        ps<_>(pos + delta * speed2, speed2)

let inline adapt (f: 'num -> 'num -> 'num) = 
    fun pos speed -> 
        let accel = f pos speed in
            sa<_>(speed, accel)

let inline runge_kutta_4 deriv_func pos speed delta =
    let half_delta = 0.5 * delta
    let sixth_delta = delta / 6.0
    let sa1: sa<_> = deriv_func pos speed
    let sa2 = deriv_func (pos + half_delta * sa1.speed) (speed + half_delta * sa1.accel)
    let sa3 = deriv_func (pos + half_delta * sa2.speed) (speed + half_delta * sa2.accel)
    let sa4 = deriv_func (pos + delta * sa3.speed) (speed + delta * sa3.accel)
    let pos1 = pos + sixth_delta*(sa1.speed + 2.0 * sa2.speed + 2.0 * sa3.speed + sa4.speed)
    let speed1 = speed + sixth_delta*(sa1.accel + 2.0 * sa2.accel + 2.0 * sa3.accel + sa4.accel) in
        ps<_>(pos1, speed1)
    
let inline simulate intg_func t0 t_fin delta pos0 speed0 =
    let oc = OptimizedClosures.FastFunc3<_,_,_,_>.Adapt(intg_func)
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let ps: ps<_> = oc.Invoke(pos0, speed0, delta)
            repeat t1 ps.pos ps.speed
    repeat t0 pos0 speed0


let initial_states = [ for s in 1..100 -> (float s, 0.0) ]    
let t0 = 0.0
let t_fin = 1000.0
let delta = 0.005

let accel = fun pos speed -> (drag 1.0) pos speed + spring pos + gravity

let ef = (euler_implicit accel)
let rk4f = (runge_kutta_4 (adapt accel))

for i in 0..3 do
    let (run_time, res) = 
        run_time_func (fun () -> 
            initial_states |> List.map(fun (x,y) ->
                simulate ef t0 t_fin delta x y))
    printfn "Euler single: %f" run_time.TotalSeconds;

    let (run_time, res) =
        run_time_func (fun () ->
            initial_states |> map_parallel(fun (x,y) ->
                simulate ef t0 t_fin delta x y))
    printfn "Euler multi: %f" run_time.TotalSeconds;

    let (run_time, res) =
        run_time_func (fun () ->
            initial_states |> List.map(fun (x,y) ->
                simulate rk4f t0 t_fin delta x y))
    printfn "RK4 single: %f" run_time.TotalSeconds;

    let (run_time, res) =
        run_time_func (fun () ->
            initial_states |>
                map_parallel(fun (x,y) ->
                    simulate rk4f t0 t_fin delta x y))
    printfn "RK4 multi: %f" run_time.TotalSeconds;
done;

ignore(Console.ReadKey(true))
