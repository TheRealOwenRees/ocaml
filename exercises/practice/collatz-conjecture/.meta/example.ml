let collatz_conjecture n =
    let rec aux n count =
        match n with
        | 1 -> Ok count
        | _ -> 
            (match n mod 2 with
                | 0 -> aux (n / 2) (count + 1)
                | _ -> aux (n * 3 + 1) (count + 1)
            )
    in
    if n < 1 then Error "Only positive integers are allowed"
    else aux n 0

