let rec last l =
    match l with
    | [] -> None
    | hd :: [] -> Some hd
    | _ :: tl -> last tl


let rec last_two l = 
    match l with
    | [] | [ _ ] -> None
    | [ a ; b ] -> Some (a, b)
    | _ :: tl -> last_two tl

let rec at k l =
    match l with
    | [] -> None
    | hd :: tl -> if k = 0 then Some hd else at (k-1) tl
