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


(*We want to check if a word is made of letters only and then we want to get the length of the words in our given input.*)
let Length_of_word a = 
    let Some_Word word rest =
        Some ( (List.length word), rest)
    let rec makeWord a acc =
        match a with
        |x::rest->                                //after converting our input into a list of chararecters e.g "hie" would be ['h';'i';'e'], 
            match System.Char.IsLetter(x) with    // a will match the list pattern and the char in the head of a will match x and will be bound to x, the tail of a will be bound to rest 
            |true->makeWord rest (x::acc)         //this is where we check if the word consists of letters only, once we reach a character that is not a letter in the char list
            |false->                              // we match with the pattern false, and in the result, we first check if the rest of the list after a non-letter char 
                match acc with                    // was encountered is an empty list, if its empty we return None else we call the Some_word function which then checks the length of the word(char list) bound to acc
                |[]->None                        // and returns a tuple of that word's length and the rest of the list.
                |_->Some_Word acc a
        |[]->Some_Word acc a
    makeWord a []
 
 
let minimum_width b =                   // here we want to make sure that the mimimum width we should start from is as long as the longest word in the input.
    b|>List.fold(fun state (length) ->
        match length > state with
        |true->length
        |_->state
    ) 1
    
let Riverwords b =                                     //here we are making sure if the input sentence is a valid string, i.e has words that contain letters and only one
    let rec RiverWords b acc =                         // space in between them. If an input sentence is a list with one word we return a None, if the input sentence is a word and space ,we return a None
        match Length_of_word b with                   //we also make sure that our input sentence has words that are not more that 80 letters in length.
        |Some (_,' '::[])->None
        |Some (v,' '::rest)-> RiverWords rest (v::acc)
        |Some (v,[])->Some (List.rev (v::acc))
        |Some _|None->None
    match RiverWords b [] with
    |Some((_::_::_) as result)->
        let width = minimum_width result
        match (width<= 80 && width>0) with
        |true-> Some result
        |false->None
    |_->None

let puting_Words_in_a_line_of_preffered_width width x =  // we then rearrange the input sentence to be in lines of the preffered width 
    let rec line x current_position acc =
        match x with 
        |v::rest->
            let newPosition = 
                match current_position = 0 with
                |true->v
                |false->1 + current_position + v
            match newPosition > width with
            |true->  (List.rev acc),x
            |false->
                match current_position = 0 with
                |true->
                    line rest newPosition acc
                |false->
                    line rest newPosition (current_position::acc)
        |[]-> (List.rev acc),[]
    List.unfold (fun state->
        match state with
        |[]->None
        |_->
            Some (line state 0 [])
    )x

let Find_longest_river lines =                        //finding the longest river
    let rec Riverpaths prev lines count =
        match lines with
        |[]->count
        | x::rest->
            match List.filter (fun ( n)-> n-1=prev||n=prev||n+1=prev) x with
            |[]-> count
            |iffound->
                List.map(fun ( n)->Riverpaths n rest (count+1)) iffound
                |> List.max
    let rec FromStart x current_max =
        match x with
        |[]->current_max
        | x::rest->
            let maximum = match List.map(fun ( n)->Riverpaths n rest 1) x with
                          |[]->0
                          |result -> List.max result
            FromStart rest (max maximum current_max)
    FromStart lines 0

let resize n x = 
    let rec checkwidth n (width,max) =                           //placing words in differentd widths and finding the maximmum river.
        match puting_Words_in_a_line_of_preffered_width n x with
        |[_]|[]->
            match 1 > max with
            |true->n,1
            |false -> width,max
        |b->
            let largestRiver = Find_longest_river b
            match largestRiver>max with
            |true->checkwidth (n+1) (n,largestRiver)
            |false->checkwidth(n+1) (width,max)
    checkwidth n (n,0)

let rivers input =
    List.ofSeq input
    |> Riverwords
    |>Option.map (fun lines->resize (minimum_width lines) lines )
    //failwith "Not implemented"


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
