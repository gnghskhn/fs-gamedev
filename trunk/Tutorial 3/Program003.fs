#light

open System

let gravity = -9.81

let euler_implicit accel pos speed delta =
    let speed2 = speed + delta * accel in
        pos + delta * speed2, speed2

(* Run a simulation by repeatedly calling an integration function.
 * t0 is the initial time, t_fin is the final time, delta is the time increment.
 * Returns the final position and speed.
 *)
let rec simulate t0 t_fin delta pos0 speed0 force =
    if t0 >= t_fin then pos0, speed0
    else
        let t1 = t0 + delta in
        let pos1, speed1 = euler_implicit force pos0 speed0 delta in
            simulate t1 t_fin delta pos1 speed1 force

let pos = 0.0 in
let speed = 0.0 in
let t0 = 0.0 in
let t_fin = 10.0 in
let delta = 1e-3 in

let pos_fin, speed_fin = simulate t0 t_fin delta pos speed gravity in
    printfn "pos = %3.3f\nspeed = %3.3f\n" pos_fin speed_fin;
    

Console.ReadKey(true)
