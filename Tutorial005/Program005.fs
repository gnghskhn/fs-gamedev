#light

open System

let gravity = 
    -9.81

let spring pos =
    let k = 10000.0 in
        -k * pos

let drag k =
    fun pos speed -> -k * speed
    
let euler_implicit accel_func pos speed delta =
    let speed2 = speed + delta * accel_func pos speed in
        pos + delta * speed2, speed2

let adapt (f: 'num -> 'num -> 'num) =
    fun pos speed -> 
        let accel = f pos speed in 
            speed, accel

let runge_kutta_4 accel_func pos speed delta =
    let half_delta = 0.5 * delta
    let deriv_func = adapt accel_func
    let s1, a1 = deriv_func pos speed
    let s2, a2 = deriv_func (pos + half_delta * s1) (speed + half_delta * a1)
    let s3, a3 = deriv_func (pos + half_delta * s2) (speed + half_delta * a2)
    let s4, a4 = deriv_func (pos + delta * s3) (speed + delta * a3)
    let pos1 = pos + delta/6.0*(s1 + 2.0*s2 + 2.0*s3 + s4)
    let speed1 = speed + delta/6.0*(a1 + 2.0*a2 + 2.0*a3 + a4) in
        pos1, speed1
                        
let simulate intg_func t0 t_fin delta pos0 speed0 =
    let rec repeat t0 t_fin pos0 speed0 =
        if t0 >= t_fin then pos0, speed0
        else
            let t1 = t0 + delta
            let pos1, speed1 = intg_func pos0 speed0 delta in
                repeat t1 t_fin pos1 speed1
    repeat t0 t_fin pos0 speed0


let pos = 0.0
let speed = 0.0
let t0 = 0.0
let t_fin = 500000.0
let delta = 0.025

let accel = fun pos speed -> (drag 100.0) pos speed + spring pos + gravity in

    let time0 = System.DateTime.Now
    let (pos_fin, speed_fin) = simulate (euler_implicit accel) t0 t_fin (0.25*delta) pos speed
    let diff0 = System.DateTime.Now - time0
    printfn "%f pos = %f speed = %f" diff0.TotalSeconds pos_fin speed_fin;

    let time0 = System.DateTime.Now
    let pos_fin, speed_fin = simulate (runge_kutta_4 accel) t0 t_fin delta pos speed
    let diff0 = System.DateTime.Now - time0
    printfn "%f pos = %f speed = %f" diff0.TotalSeconds pos_fin speed_fin

ignore(Console.ReadKey(true))
