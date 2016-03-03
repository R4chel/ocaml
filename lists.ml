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

let rec length l =
    match l with
    | [] -> 0
    | _ :: tl -> 1 + length tl

let rev l =
    let rec aux l1 l2 = 
        match l1 with 
        | [] -> l2
        | hd :: tl -> aux tl (hd::l2)
    in
    aux l []


let is_palindrome l =
    l = rev l
