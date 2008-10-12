#light

let gravity = GenericPhysics.mk_gravity <@ fun x y -> x * y @> 1.0
let drag = GenericPhysics.mk_drag <@ fun x y -> x * y @>
let spring = GenericPhysics.mk_spring <@ fun x y -> x * y @>
let rk4 = GenericPhysics.mk_rk4 <@ fun x y -> x + y @> <@ fun x y -> x * y @>
let euler = GenericPhysics.mk_euler_implicit <@ fun x (y:float) -> x + y @> <@ fun x y -> x * y @>
