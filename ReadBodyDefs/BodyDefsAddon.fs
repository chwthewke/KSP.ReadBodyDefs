namespace ReadBodyDefs

open UnityEngine

[<KSPAddon(KSPAddon.Startup.TrackingStation, true)>]
type BodyDefsAddon() =
    inherit MonoBehaviour()
    member self.Start () : unit =
        MonoBehaviour.print "[ReadBodyDefs] LOADED"
