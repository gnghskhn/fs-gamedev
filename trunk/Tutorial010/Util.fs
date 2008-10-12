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