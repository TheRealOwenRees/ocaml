open Base

let earth_years seconds = seconds /. 31557600.0
let rel_years = function
    | "Mercury" -> 0.2408467
    | "Venus"   -> 0.61519726
    | "Mars"    -> 1.8808158
    | "Earth"   -> 1.
    | "Jupiter" -> 11.862615
    | "Saturn"  -> 29.447498
    | "Uranus"  -> 84.016846
    | "Neptune" -> 164.79132
    | _         -> 1.

let age_on planet seconds =
    let seconds' = Float.of_int seconds in
    match planet with
    | "Mercury" | "Venus" | "Earth" | "Mars" | "Jupiter" | "Saturn" | "Uranus" | "Neptune" -> Ok (earth_years seconds' /. rel_years planet)
    | _ -> Error "not a planet"
