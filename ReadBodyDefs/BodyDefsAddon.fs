namespace ReadBodyDefs

open UnityEngine

[<KSPAddon(KSPAddon.Startup.TrackingStation, true)>]
type BodyDefsAddon() =
    inherit MonoBehaviour()
    member self.Start () : unit =
        MonoBehaviour.print "[ReadBodyDefs] LOADED"

module Objects =
    let toOption (obj: 'a): 'a option = if obj = null then None else Some obj

module BodyData =
    type Body = 
        { Name: string
        ; Radius: float
        ; RotationPeriod: float
        ; GravitationalParameter: float
        ; SoI: float option
        }

    let ReadSoI : CelestialBody -> float option = fun cb ->
        let soi = cb.sphereOfInfluence
        in if (soi = 0.0) then None else Some(soi) // Check presence condition

    let ReadColor : CelestialBody -> Color option = fun cb ->
        cb.orbitDriver 
            |> Objects.toOption
            |> Option.bind (fun d -> d.Renderer |> Objects.toOption)
            |> Option.map (fun r -> r.orbitColor)

    let Read : CelestialBody -> Body = fun cb ->
        { Name = cb.bodyName
        ; Radius = cb.Radius
        ; RotationPeriod = cb.rotationPeriod
        ; GravitationalParameter = cb.gravParameter
        ; SoI = ReadSoI cb
        }

