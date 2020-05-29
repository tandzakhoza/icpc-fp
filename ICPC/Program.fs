module ICPC
open System

let length v =
    match String.length v >= 2 with
    |true -> Some v
    |false -> None

let containCapital v =
    let charlist = List.ofSeq v
    let rec islower list=
        match list with
        |[] -> Some v
        |h::rest-> 
            match Char.IsLower(h) with
            |true -> islower rest
            |false -> None
    islower charlist

let beginswith v =
    let xs = List.ofSeq v
    match xs with
    | ['.'] -> None
    | [' '] | ['.';' '] | [',';' '] -> None
    | '.' :: ' ' :: rest -> None
    | ',' :: ' ' :: rest -> None
    | ',' :: rest -> None
    | ' ' :: rest -> None
    | ' ' :: rest -> None
    | _ -> Some v

let middleofwords xs=
    match xs with
    //| [' '] | ['.';' '] | [',';' '] -> None
    | '.' :: ' ' :: rest -> Some ("period", rest)
    | ',' :: ' ' :: rest -> Some ("comma", rest)
    | ' ' :: rest -> Some ("space", rest)
    | _ -> None


let bindpunc v =
    let xs = List.ofSeq v
    let rec puncuation list acc word=
        match list with
        |a::b::rest -> 
        // MATCH FINDIN A PUNC IE COMMA 
        //"please sit spot. sit spot, sit. spot here now here."
        //"please, sit spot. sit spot, sit. spot, here now, here."
            match middleofwords list with
            |None-> puncuation (b::rest) (a::acc) (a::word)
            |Some ("space", c)-> puncuation c (a::acc) []
            |Some ("period", d)-> puncuation d (a::acc) []
            |Some ("comma", e)-> puncuation [] (a::acc) word
        |[]-> List.rev word
    puncuation xs [] [] 




let bind v  =
    match length v with
    |Some a -> 
        match containCapital v with
        |None -> 
            match beginswith v with
            |Some _ -> 
                let xs= List.ofSeq v
                let rec PuncInSentence list acc i=
                    match middleofwords xs,i%2 =0 with 
                    |Some (punc, []),false -> Some (List.rev (punc::acc)) // string list option of punc
                    |Some (punc, rest),false -> PuncInSentence rest (punc::acc) (i+2)
                    |None, _-> None
                    |_-> Some (List.fold (fun acc v -> string v::acc) [] xs)//gives
                PuncInSentence xs [] 1
            |None -> None
        |_ -> None
    |None -> None 


let commaSprinkler input =
    bind input



   // failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
