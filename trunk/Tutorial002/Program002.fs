#light

open System

(* The acceleration due to gravity *)
let gravity = -9.81

(*
 * Euler integration.
 * pos += delta * tmp;
 * speed += delta * accel;
 *)
let euler_explicit accel pos speed delta =
    pos + delta * speed, speed + delta * accel

(*
 * Euler integration, as typically implemented by game programers.
 * speed += delta * acceleration;
 * pos += delta * speed;
 *)
let euler_implicit accel pos speed delta =
    let speed2 = speed + delta * accel in
        pos + delta * speed2, speed2

(* Call euler_explicit with null initial position and speed, delta = 1s *)
let pos, speed = euler_explicit gravity 0.0 0.0 1.0 in
    printfn "pos = %f speed = %f\n" pos speed

(* Call euler_implicit with null initial position and speed, delta = 1s *)    
let pos, speed = euler_implicit gravity 0.0 0.0 1.0 in
    printfn "pos = %f speed = %f\n" pos speed

Console.ReadKey(true)
