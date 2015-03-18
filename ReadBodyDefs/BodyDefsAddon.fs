namespace ReadBodyDefs

open UnityEngine
open KSP.IO

module Utils =
    let toOption (obj: 'a): 'a option = if obj = null then None else Some obj
    let cata (f: 'a -> 'b) = Option.fold (fun _ x -> f x)

module Bodies =
    type OrbitData = 
        { OrbitParent : string
        ; SemiMajorAxis : float
        ; Eccentricity : float
        ; LongitudeOfAscendingNode : float
        ; Inclination : float
        ; ArgumentOfPeriapsis : float
        ; MeanAnomalyAtEpoch : float
        ; OrbitColor : Color
        }

    type AtmosphereData = 
        { Pressure : float
        ; ScaleHeight : float
        ; MaxHeight : float
        }

    type BodyData = 
        { Name: string
        ; Radius: float
        ; RotationPeriod: float
        ; GravitationalParameter: float
        ; SoI: float option
        ; Orbit : OrbitData option
        ; Atmosphere : AtmosphereData option
        }

    let ReadSoI : CelestialBody -> float option = fun cb ->
        let soi = cb.sphereOfInfluence
        in if (System.Double.IsInfinity(soi)) then None else Some(soi)

    let ReadOrbit : CelestialBody -> OrbitData option =
        let ReadOrbitData : OrbitDriver -> OrbitData = fun orb ->
            { OrbitParent = orb.orbit.referenceBody.bodyName
            ; SemiMajorAxis = orb.orbit.semiMajorAxis
            ; Eccentricity = orb.orbit.eccentricity
            ; LongitudeOfAscendingNode = orb.orbit.LAN
            ; Inclination = orb.orbit.inclination
            ; ArgumentOfPeriapsis = orb.orbit.argumentOfPeriapsis
            ; MeanAnomalyAtEpoch = orb.orbit.meanAnomalyAtEpoch
            ; OrbitColor = orb.orbitColor
            }
        in
        fun cb -> cb.orbitDriver |> Utils.toOption |> Option.map ReadOrbitData

    let ReadAtmosphere : CelestialBody -> AtmosphereData option =
        let ReadAtmosphereData : CelestialBody -> AtmosphereData = fun cb ->
            { Pressure = float cb.atmosphereMultiplier * 101.325
            ; ScaleHeight = cb.atmosphereScaleHeight * 1000.0
            ; MaxHeight = float cb.maxAtmosphereAltitude
            }
        in
        fun cb -> if cb.atmosphere then Some (ReadAtmosphereData cb) else None
            
    let Read : CelestialBody -> BodyData = fun cb ->
        { Name = cb.bodyName
        ; Radius = cb.Radius
        ; RotationPeriod = cb.rotationPeriod
        ; GravitationalParameter = cb.gravParameter
        ; SoI = ReadSoI cb
        ; Orbit = ReadOrbit cb
        ; Atmosphere = ReadAtmosphere cb
        }

module DataFormat =

    let FmtName : Bodies.BodyData list -> Bodies.BodyData -> string = fun bs b ->
        let l = bs |> List.map (fun b -> b.Name.Length) |> List.max
        in b.Name + String.replicate (l - b.Name.Length) " "

    let PrintBodyIds : Bodies.BodyData list -> string = 
        List.map (fun b -> b.Name) >> String.concat "\n    | " >> sprintf "    %s\n"

    
    let PrintAtmo : Bodies.BodyData list -> Bodies.BodyData -> Bodies.AtmosphereData -> string option = fun bs b atmo ->
        sprintf "getAtmosphere %s = mkAtmosphere %.0f %.5f %.0f\n" (FmtName bs b) atmo.MaxHeight atmo.Pressure atmo.ScaleHeight |> Some

    let PrintBodyAtmo : Bodies.BodyData list -> Bodies.BodyData -> string option = fun bs b ->
        Utils.cata (PrintAtmo bs b) None b.Atmosphere

    let PrintAllAtmo : Bodies.BodyData list -> string = fun bs ->
        bs  |> List.map (PrintBodyAtmo bs >> Option.toList) 
            |> List.concat 
            |> String.concat "" 
            |> (fun s -> s + "getAtmosphere _ = Nothing\n")

    let PrintPhysicalAttrs : Bodies.BodyData list -> Bodies.BodyData -> string = fun bs b ->
        sprintf "getPhysicalAttrs %s = mkPhysicalAttrs %.0f %.3f %.7e\n" (FmtName bs b) b.Radius b.RotationPeriod b.GravitationalParameter

    let PrintAllPhysicalAttrs : Bodies.BodyData list -> string = fun bs ->
        bs |> List.map (PrintPhysicalAttrs bs) |> String.concat ""

    let PrintSphereOfInfluence : Bodies.BodyData list -> Bodies.BodyData -> string = fun bs b ->
        let PrintSoI = Utils.cata (sprintf "Just $ %.7e *~ meter") "Nothing"
        in sprintf "getSphereOfInfluence %s = %s\n" (FmtName bs b) (PrintSoI b.SoI)

    let PrintAllSphereOfInfluence : Bodies.BodyData list -> string = fun bs ->
        bs |> List.map (PrintSphereOfInfluence bs) |> String.concat ""

    let PrintColor : Bodies.BodyData list -> Bodies.BodyData -> string = fun bs b ->
        let PrintColor' : Color option -> string = Utils.cata (fun c -> sprintf "Just $ RgbaFColor %f %f %f %f" c.r c.g c.b c.a) "Nothing"
        in b.Orbit |> Option.map (fun o -> o.OrbitColor) |> PrintColor' |> sprintf "getColor %s = %s\n" (FmtName bs b)

    let PrintAllColor : Bodies.BodyData list -> string = fun bs ->
        bs |> List.map (PrintColor bs) |> String.concat ""

    let PrintOrbit : int -> Bodies.BodyData -> string = fun l b ->
        let FmtName n = n + String.replicate (l - n.Length) " "
        let lead = String.replicate (32 + 2 * l) " "
        let PrintOrbit' : Bodies.OrbitData -> string = fun o ->
            sprintf "_bodyOrbit %s = Just $ classical %s (%.0f *~ meter)\n%s(%f *~ one)\n%s(%.2f *~ degree)\n%s(%.2f *~ degree)\n%s(%.2f *~ degree)\n%s(%.2f *~ radian)\n"
                (FmtName b.Name) (FmtName o.OrbitParent) o.SemiMajorAxis lead o.Eccentricity lead o.LongitudeOfAscendingNode lead o.Inclination lead o.ArgumentOfPeriapsis lead o.MeanAnomalyAtEpoch
        in Utils.cata PrintOrbit' (sprintf "_bodyOrbit %s = Nothing\n" (FmtName b.Name)) b.Orbit

    let PrintAllOrbit : Bodies.BodyData list -> string = fun bodies ->
        let l = bodies |> List.map (fun b -> b.Name.Length) |> List.max
        in bodies |> List.map (PrintOrbit l) |> String.concat ""

    let PrintData : Bodies.BodyData list -> string = fun bodies ->
        "\n-- BodyIds\n" +
            PrintBodyIds bodies +
            "\n-- Atmospheres\n" +
            PrintAllAtmo bodies +
            "\n-- Physical attributes\n" +
            PrintAllPhysicalAttrs bodies +
            "\n-- Spheres of influence\n" +
            PrintAllSphereOfInfluence bodies +
            "\n-- Colors\n" +
            PrintAllColor bodies +
            "\n-- Orbits\n" +
            PrintAllOrbit bodies

            
[<KSPAddon(KSPAddon.Startup.TrackingStation, true)>]
type BodyDefsAddon() =
    inherit MonoBehaviour()
    member self.Start () : unit =
        MonoBehaviour.print "[ReadBodyDefs] LOADED"
        let cbs = List.ofSeq FlightGlobals.Bodies in
        let bd = cbs |> List.map Bodies.Read |> DataFormat.PrintData
        in 
            MonoBehaviour.print bd;
            using (TextWriter.CreateForType<BodyDefsAddon>("BodyDefs.hsx")) (fun tw -> tw.Write bd)

