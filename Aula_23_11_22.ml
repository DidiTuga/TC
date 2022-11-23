
(*[Kwd IF; Ident x; Kwd THEN; Int 5; Kwd ELSE; Int 10]*)

type kwd = 
    | IF
    | THEN
    | ELSE

type token =
    | Kwd of kwd
    | Ident of string
    | Int of int
    | Error of string

let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

let is_number n = List.for_all (fun c -> is_digit c) (List.of_seq (String.to_seq n))

let is_ident i = 
    let list = List.of_seq (String.to_seq i) in
    if is_alpha (List.hd list) then
        List.for_all (fun c -> is_alpha c || is_digit c) (List.tl list)
    else
        false


let string_of_kwd = function
    | IF -> "IF"
    | THEN -> "THEN"
    | ELSE -> "ELSE"
let token_of_string = function
    | "if" -> Kwd IF
    | "then" -> Kwd THEN
    | "else" -> Kwd ELSE
    | s when (is_number s) -> Int (int_of_string s)
    | s when (is_ident s) -> Ident s
    | _ as e -> Error e

let string_of_token = function
    Kwd k -> "kwd" ^ (string_of_kwd k)
    | Ident i -> "ident" ^ i
    | Int i -> "int" ^ string_of_int i
    | Error e -> "error" ^ e

let () =
    let buffer = String.split_on_char ' ' (read_line ()) in
    let tokens = List.map token_of_string buffer in
    List.iter (fun t -> print_endline (string_of_token t)) tokens
    