// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let insert0 e l = l |> List.map (fun x-> e::x)
let rec insert1 e l = match l with
                        |[] -> [[e]]
                        |(x::xs) as g -> (e::g)::insert0 x (insert1 e xs)
let rec perm l = match l with
                   [] -> []
                   |[e1;e2]->[[e1;e2];[e2;e1]]
                   |(x::xs)->List.collect (insert1 x) (perm xs)