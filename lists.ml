let rec last l =
    match l with
    | [] -> None
    | hd :: [] -> Some hd
    | hd :: tl -> last tl
