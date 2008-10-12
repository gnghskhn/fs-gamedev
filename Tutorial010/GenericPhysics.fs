#light

open System
open Microsoft.FSharp.Linq.QuotationEvaluation 

#nowarn "57"


type ps<'Vec> = class
    val pos:'Vec
    val speed:'Vec
    new(pos, speed) = { pos = pos; speed = speed }
end


type sa<'Vec> = class
    val speed:'Vec
    val accel:'Vec
    new(speed, accel) = { speed = speed; accel = accel }
end

        
let mk_gravity scale_func (up: 'Vec): 'Vec =
    let q = 
        <@
        let (*) = %scale_func in -9.81 * up
        @>
    q.Eval()


let mk_spring scale_func: float -> 'Vec -> 'Vec =
    let q =
        <@
        let (*) = %scale_func in fun k v -> -k * v
        @>
    q.Eval()


let mk_drag scale_func: float -> 'Vec -> 'Vec =
    let q =
        <@
        let (*) = %scale_func in fun k v -> -k * v
        @>
    q.Eval()

    
let mk_euler_implicit plus_func scale_func =
    let q =
        <@
        let (+) = %plus_func in
        let (*) = %scale_func in
        fun accel_func pos speed delta ->
            let speed2 = speed + delta * (accel_func pos speed)
            let pos2 = pos + delta * speed2
            ps<_>(pos2, speed2)
        @>
    q.Eval()

    
let inline adapt (f: 'Vec -> 'Vec -> 'Vec) = 
    fun pos speed -> 
        let accel = f pos speed in
            sa<_>(speed, accel)


let mk_rk4_q plus_func scale_func =
    <@
    fun deriv_func pos speed delta ->
        let pf = %plus_func
        let sf = %scale_func
        
        let extrapolate x delta dx = 
            pf x (sf delta dx)

        let half_delta = 0.5 * delta
         
        let sa1: sa<_> = deriv_func pos speed
        let sa2 = deriv_func (extrapolate pos half_delta sa1.speed) (extrapolate speed half_delta sa1.accel)
        let sa3 = deriv_func (extrapolate pos half_delta sa2.speed) (extrapolate speed half_delta sa2.accel)
        let sa4 = deriv_func (extrapolate pos delta sa3.speed)      (extrapolate speed delta sa3.accel)

        let sixth_delta = delta / 6.0    

        let update old v1 v2 v3 v4 =
            pf old (sf sixth_delta
                (pf v1
                (pf v2
                (pf v2
                (pf v3
                (pf v3
                    v4))))))
                             
        let pos1 = update pos sa1.speed sa2.speed sa3.speed sa4.speed
        let speed1 = update speed sa1.accel sa2.accel sa3.accel sa4.accel
        
        ps<'Vec>(pos1, speed1)
    @>


let mk_rk4 plus_func scale_func =
    let q = mk_rk4_q plus_func scale_func
    q.Eval()

    
let simulate intg_func (t0: float) t_fin delta pos0 speed0 =
    let rec repeat t0 pos0 speed0 =
        if t0 > t_fin then (pos0, speed0)
        else
            let t1 = t0 + delta
            let ps: ps<_> = intg_func pos0 speed0 delta
            repeat t1 ps.pos ps.speed
    repeat t0 pos0 speed0


