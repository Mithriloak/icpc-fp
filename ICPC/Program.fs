module ICPC 
open System


//Input Validation:
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
 
let rec CheckFirst (input : string) = //checks that the first letters and the rest of the sentence are chars
    match Char.IsLetter(input, 0) with 
        | true -> true
        | _ -> false

let FullstopCheck (input:string) = //variant of the full stop check (higher efficiency.)
    match input.Substring((input.Length - 1)) = "." with      
    |true -> true
    | _ -> false

 
let rec SpaceCheck input = //checking full stops, spaces and comma rules
    match input with
    | x::tail -> match x with 
                 | '.' -> match tail with 
                          | [] -> false
                          | x::tail -> match x with
                                       | ' ' -> match tail with 
                                                | [] -> false
                                                | x::tail -> match x with
                                                             |'a' | 'b' | 'c' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> SpaceCheck tail
                                                             | _ -> false
                                       | _ -> false
                          

                 |',' -> match tail with 
                        |[] -> false 
                        |x::tail -> match x with 
                                    |' ' -> match tail with     
                                            |[]-> false
                                            |x::tail -> match x with
                                                            |'a' | 'b' | 'c' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> SpaceCheck tail
                                                            | _ -> false
                                    | _ -> false
                            
                 
                 | ' ' -> match tail with  
                          | [] -> false
                          | x::tail -> match x with
                                       |'a' | 'b' | 'c' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> SpaceCheck tail     
                                       | _ -> false         
                 | _ -> SpaceCheck tail
                 
    | [] -> true
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//


let Checkinput (input:string) = //checks all the inputs at once 
    match input.Length >= 2 with
    |false -> None
    |true -> let x = Seq.toList input 
             match CheckFirst input with
                    |false -> None
                    |true -> match SpaceCheck x with
                             |false -> None
                             |true -> match FullstopCheck input with
                                      |false -> None
                                      |true -> Some input

   
                                    
let commaSprinkler input =
    match Checkinput input with
    |None -> None
    |Some input -> Some(input)

let rivers (input:string) = //rivers input check
  let length = input.Length

  match length=0 with
  |true -> None
  |false-> match length <> 0 || input.Contains('!') || input.Contains(',') || input.[input.Length-1]= ' ' || input.[0] = ' ' with
          |true -> None
          |_ -> match  input.Contains(' ') with
              |false-> None
              |true -> Some input.Length 

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code