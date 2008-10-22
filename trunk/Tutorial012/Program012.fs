#light

open System

(*
 * Definitions
 *)
 
// A 3d-vector
type Vec3d = class
    val x: float
    val y: float
    val z: float
    
    new (x)       = { x = x; y = 0.0; z = 0.0 }
    new (x, y, z) = { x = x; y = y;   z = z   } 
end

// Addition of two vectors.
let plus_Vec3d (a: Vec3d, b: Vec3d) = Vec3d(a.x + b.x, a.y + b.y, a.z + b.z )

// Scale a vector by a float.
let scale_Vec3d (k: float, a: Vec3d) = Vec3d(k * a.x, k * a.y, k * a.z )

// Addition of many vectors: avoid creating vectors for each intermediate result.
let add6 ((v0: Vec3d), (v1: Vec3d), (v2: Vec3d), (v3: Vec3d), (v4: Vec3d), (v5: Vec3d)) =
    let x = v0.x + v1.x + v2.x + v3.x + v4.x + v5.x
    let y = v0.y + v1.y + v2.y + v3.y + v4.y + v5.y
    let z = v0.z + v1.z + v2.z + v3.z + v4.z + v5.z
    
    Vec3d(x, y ,z)

// Functions performing the sum of 6 vectors.
type Add6<'vector> = 'vector * 'vector * 'vector * 'vector * 'vector * 'vector -> 'vector

// Dictionary of operations.
type VecOps<'vector, 'scalar> = {
    up: 'vector
    add: 'vector * 'vector -> 'vector
    add6: Add6<'vector> option
    scale: 'scalar * 'vector -> 'vector
}

// Final step in runge-kutta 4
let inline average (vec_ops: VecOps<_, _>) old v1 v2 v3 v4 =
    let (*) k v = vec_ops.scale(k, v)
    let (+) u v = vec_ops.add(u, v)
    
    match vec_ops.add6 with
    // No addMany available, use pair-wise additions (will create many intermediate vectors).
    | None ->
        old + (1.0/6.0) * (v1 + 2.0 * (v2 + v3) + v4)
    // addMany available, use it.
    | Some add ->
        let tmp = add (v1, v2, v2, v3, v3, v4)
        old + (1.0/6.0) * tmp

// Same as "average", avoiding the dictionary
let inline average2 (scale: float * 'vec -> 'vec) (add: 'vec * 'vec -> 'vec) (add6: 'vec Add6 option) old v1 v2 v3 v4 =
    let (*) k v = scale(k, v)
    let (+) u v = add(u, v)
    
    match add6 with
    // No addMany available, use pair-wise additions (will create many intermediate vectors).
    | None ->
        old + (1.0/6.0) * (v1 + 2.0 * (v2 + v3) + v4)
    // addMany available, use it.
    | Some addM ->
        let tmp = addM(v1, v2, v2, v3, v3, v4)
        old + (1.0/6.0) * tmp

(* A record holding the instantiated form of average2.
   The only interesting field is named "average", the "dummy" fields are just there
   to convey the intent of InstantiatedFuncs *)
type InstantiatedFuncs<'Vector> = {
    average : 'Vector -> 'Vector -> 'Vector -> 'Vector -> 'Vector -> 'Vector
    dummy1 : 'Vector -> 'Vector -> 'Vector -> 'Vector -> 'Vector
    dummy2 : 'Vector -> 'Vector -> 'Vector -> 'Vector
    dummy3 : 'Vector -> 'Vector -> 'Vector
    dummy4 : 'Vector -> 'Vector
    dummy5 : 'Vector -> 'Vector    
}

(* An empty class used to create instances of InstantiatedFuncs.
   Using a class allows to take advantage of named parameters and optional parameters. *)
type AverageWrapper<'Vector> =
    static member inline instantiate(scale, add2, ?add6) =
        {average = average2 scale add2 add6;
        dummy1 = (fun v1 v2 v3 v4 -> average2 scale add2 add6 v1 v1 v2 v3 v4);
        dummy2 = (fun v1 v2 v3 -> average2 scale add2 add6 v1 v1 v1 v2 v3);
        dummy3 = (fun v1 v2 -> average2 scale add2 add6 v1 v1 v1 v1 v2);
        dummy4 = (fun v1 -> average2 scale add2 add6 v1 v1 v1 v1 (add2(v1, v1)));
        dummy5 = (fun v1 -> average2 scale add2 add6 v1 v1 (add2(v1, v1)) v1 v1)}
                
(*
 * Main program: Instantiate "average" and "average2", then measure their performance
 *)
  
// Dictionary of operations for Vec3d
let Vec3d_ops = {
    add = plus_Vec3d;
    scale = scale_Vec3d;
    add6 = Some add6;
    up = Vec3d(1.0)
}

// "average" instantiated for Vec3d, with specialized addition of 6 vectors.
let average_Vec3d = average Vec3d_ops 
// "average" instantiated for Vec3d, without specialized addition of 6 vectors.
let average_Vec3d' = average {Vec3d_ops with add6 = None} 

// Same as above, for "average2"
let average2_Vec3d = average2 scale_Vec3d plus_Vec3d (Some add6)
let average2_Vec3d' = average2 scale_Vec3d plus_Vec3d None

// Using AverageWrapper
type AW_Vec3d = AverageWrapper<Vec3d>
let funcs = AW_Vec3d.instantiate(scale = scale_Vec3d, add2 = plus_Vec3d, add6 = add6)
let funcs' = AW_Vec3d.instantiate(scale = scale_Vec3d, add2 = plus_Vec3d)
let average3_Vec3d = funcs.average
let average3_Vec3d' = funcs'.average

// Instantiations for floats.
let float_ops = {
    add = fun (x, y) -> x + y
    scale = fun (x, y) -> x * y
    add6 = Some (fun (v0, v1, v2, v3, v4, v5) -> v0 + v1 + v2 + v3 + v4 + v5)
    up = 1.0;
}

let average_float = average float_ops
let average_float' = average {float_ops with add6 = None }

let average2_float = average2 (fun (x,y) -> x*y) (fun (x,y) -> x+y) (Some (fun (v0, v1, v2, v3, v4, v5) -> v0 + v1 + v2 + v3 + v4 + v5))
let average2_float' = average2 (fun (x,y) -> x*y) (fun (x,y) -> x+y) None

// Perform a large number of calls to a function, measuring the execution time.
let run_timed func old v1 v2 v3 v4 =
    let rec work i old v1 v2 v3 v4 =
        match i with
        | 0 -> func old v1 v2 v3 v4
        | _ -> let v5 = func old v1 v2 v3 v4 in work (i-1) old v2 v3 v4 v5
        
    let watch = new System.Diagnostics.Stopwatch()
    watch.Start()
    let res = work 10000000 old v1 v2 v3 v4
    watch.Stop()
    let diff0 = float watch.ElapsedMilliseconds / 1000.0
    (diff0, res)

// Prepare the runs for average<Vec3d>
let old = Vec3d(0.0)
let v1 = Vec3d(10000.0)
let v2 = Vec3d(5000.0)
let v3 = Vec3d(5000.0)
let v4 = Vec3d(3000.0)

// Run average<Vec3d> and its variants.
let t, res = run_timed average_Vec3d old v1 v2 v3 v4 in
    printf "Vec3d average   %f %f\n" t res.x

let t, res = run_timed average_Vec3d' old v1 v2 v3 v4 in
    printf "Vec3d average'  %f %f\n" t res.x

let t, res = run_timed average2_Vec3d old v1 v2 v3 v4 in
    printf "Vec3d average2  %f %f\n" t res.x

let t, res = run_timed average2_Vec3d' old v1 v2 v3 v4 in
    printf "Vec3d average2' %f %f\n" t res.x

let t, res = run_timed average3_Vec3d old v1 v2 v3 v4 in
    printf "Vec3d average3  %f %f\n" t res.x

let t, res = run_timed average3_Vec3d' old v1 v2 v3 v4 in
    printf "Vec3d average3' %f %f\n" t res.x

// Run average<float> and its variants.
let t, res = run_timed average_float 0.0 10000.0 5000.0 5000.0 3000.0 in
    printf "float average   %f %f\n" t res

let t, res = run_timed average_float' 0.0 10000.0 5000.0 5000.0 3000.0 in
    printf "float average'  %f %f\n" t res

let t, res = run_timed average2_float 0.0 10000.0 5000.0 5000.0 3000.0 in
    printf "float average2  %f %f\n" t res

let t, res = run_timed average2_float' 0.0 10000.0 5000.0 5000.0 3000.0 in
    printf "float average2' %f %f\n" t res

