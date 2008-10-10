#light

open System

let gravity = 
    -9.81

let spring pos =
    let k = 10.0 in
        -k * pos

let drag k =
    fun speed -> -k * speed
    
let euler_implicit accel pos speed delta =
    let speed2 = speed + delta * accel in
        pos + delta * speed2, speed2
                
let rec simulate t0 t_fin delta pos0 speed0 accel_func =
    if t0 >= t_fin then pos0, speed0
    else
        let t1 = t0 + delta
        let accel = accel_func pos0 speed0
        let pos1, speed1 = euler_implicit accel pos0 speed0 delta in
            simulate t1 t_fin delta pos1 speed1 accel_func

let pos = 0.0
let speed = 0.0
let t0 = 0.0
let t_fin = 10.0
let delta = 1e-3

let accel = fun pos speed -> (drag 1.0) speed + spring pos + gravity

let pos_fin, speed_fin = simulate t0 t_fin delta pos speed accel
printfn "pos = %3.3f\nspeed = %3.3f\n" pos_fin speed_fin;

ignore(Console.ReadKey(true))