module ICPC
open System



let FirstLetter x = match x with //checking that first letter is a lower case
                    |[] -> false
                    |x::rest -> match x with
                                |'a' | 'b' | 'c' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> true
                                | _ -> false                         

let rec CharCheck x = match x with //checking that each char is either a lower case or a space, comma or full stop
                      |[] -> true
                      |y::rest -> match y with 
                                 |'a' | 'b' | 'c' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'| ' ' | ',' | '.' -> CharCheck rest
                                 | _ -> false
 
let rec Fullstopcheck x = match x with //checking that the sentence ends in a full stop
                          |y::rest -> match rest with
                                      |[] -> match y with
                                             | '.' -> true
                                             | _ -> false
                                      | _ -> Fullstopcheck rest
                          |[] -> false
 
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



let Checkinput (input:string) = //checks all the inputs at once 
    match input.Length >= 2 with
    |false -> None
    |true -> let x = Seq.toList input 
             match FirstLetter x with
             |false -> None
             | _ -> match CharCheck x with
                    |false -> None
                    |true -> match SpaceCheck x with
                             |false -> None
                             |true -> match Fullstopcheck x with
                                      |false -> None
                                      |true -> Some input

   
                                    
let commaSprinkler input =
    Checkinput input

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