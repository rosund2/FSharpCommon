#r "nuget: FSharpPlus"

open FSharpPlus;


let whenSomeandThenSome a b = 
    match a with 
    | Some a -> 
        match b with 
        | Some b -> Some ( a + b )
        | None -> None
    | None -> None



(Some 2, None)
||> whenSomeandThenSome

let m = [("email","ahahah"); ("postNo","noway")]|> Map.ofSeq


type Validation<'T,'E> = 
    | Ok of 'T
    | Fail of 'E

type Error = 
    | ValidationError

let validateEmail (m:Map<string,string>) = 
    match m.ContainsKey "email" with 
    | true -> Ok m
    | false -> Fail  "email error"

let validatePostNo (m:Map<string,string>) = 
    match m.ContainsKey "postNo2" with 
    | true -> Ok m
    | false -> Fail  "postNoError"

// previous was either ok or failure

let validate f = 
    // returns a function that takes a previous validation v
    // that executes f and returns either a Ok or Fail depending on v
    // takes to args previous validation and current
    // Should return a tuple right or how do we now when 
    // valueToProcess, previousValidation<Ok, Fail>
    (fun (vtp, pv) -> 
        match pv with
        | Ok m -> 
            let q = f m
            match q with 
            | Ok _ -> (q, Ok m)
            | Fail l -> (q, Ok m)
        | Fail _ ->  
            let q = f m
            match q with 
            | Ok m -> (q, Ok m)
            | Fail _  ->  (q, Ok m)
    )



//let q =  
//    m
//    // I have a map and i validateTheEmail
//    // So first is just either a Failure or Ok       
//    |!> validate validateEmail
//    ||> validate validatePostNo
    