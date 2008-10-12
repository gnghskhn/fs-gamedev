#light

open System
open FloatPhysics
open Util

let inline accel pos speed =
    drag 1.0 speed + spring 100.0 pos + gravity

let ef = (FloatPhysics.euler accel)
let rk4f = (rk4 (GenericPhysics.adapt accel))

let run euler_func s0 =
    let t0 = 0.0
    let t_fin = 1000.0
    let delta = 0.005

    let run_timed map_func = 
        let n_runs = 2
        let times =
            [
                for i in 1..n_runs ->
                    let (run_time, res) =
                        run_time_func (fun () -> 
                            s0 |> map_func(fun (x, y) -> 
                                GenericPhysics.simulate euler_func t0 t_fin delta x y))
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


let n_bodies = 2

let s0_float = [ for s in 1..n_bodies -> (float s, 0.0) ]
run ef s0_float
run rk4f s0_float

ignore(Console.ReadKey(true))
