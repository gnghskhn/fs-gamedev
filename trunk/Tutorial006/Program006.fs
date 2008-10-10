#light

open System

let gravity = 
    -9.81

let inline spring pos =
    let k = 100.0 in
        -k * pos

let inline drag k pos speed = -k * speed
    
let inline euler_implicit accel_func pos speed delta =
    let speed2 = speed + delta * accel_func pos speed in
        pos + delta * speed2, speed2

let inline adapt (f: 'num -> 'num -> 'num) = 
    fun pos speed -> 
        let accel = f pos speed in
            speed, accel

let inline runge_kutta_4 deriv_func pos speed delta =
    let half_delta = 0.5 * delta
    let sixth_delta = delta/6.0
    let s1, a1 = deriv_func pos speed
    let s2, a2 = deriv_func (pos + half_delta * s1) (speed + half_delta * a1)
    let s3, a3 = deriv_func (pos + half_delta * s2) (speed + half_delta * a2)
    let s4, a4 = deriv_func (pos + delta * s3) (speed + delta * a3)
    let pos1 = pos + sixth_delta*(s1 + 2.0*s2 + 2.0*s3 + s4)
    let speed1 = speed + sixth_delta/6.0*(a1 + 2.0*a2 + 2.0*a3 + a4) in
        pos1, speed1
    
let inline simulate intg_func t0 t_fin delta pos0 speed0 =
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let pos1, speed1 = intg_func pos0 speed0 delta
            repeat t1 pos1 speed1
    repeat t0 pos0 speed0


let initial_states = [| for s in [1..1000] -> (float s * 1.0, 0.0) |]    
let t0 = 0.0
let t_fin = 1000.0
let delta = 0.025

let accel pos speed = (drag 1.0) pos speed + spring pos + gravity in

    let time0 = System.DateTime.Now
    let final_states = initial_states |> Array.map(fun (x,y) -> simulate (euler_implicit accel) t0 t_fin (0.25*delta) x y)
    let diff0 = System.DateTime.Now - time0
    printfn "Time: %f Result: %A" diff0.TotalSeconds final_states;
    
    let time0 = System.DateTime.Now
    let final_states = initial_states |> Array.map(fun (x,y) -> simulate (runge_kutta_4 (adapt accel)) t0 t_fin delta x y)
    let diff0 = System.DateTime.Now - time0
    printfn "Time: %f Result: %A" diff0.TotalSeconds final_states

ignore(Console.ReadKey(true))
