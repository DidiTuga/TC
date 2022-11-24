(*add some comment*)
open List
open Scanf
open Array

let n = Scanf.scanf " %d" (fun a -> a)

let i = Scanf.scanf " %d" (fun a -> a)

let iniciais = 
  let l = ref [] in
  (* Lê f numeros, os estados finais *)
  for i = 0 to i-1 do
    (* Descarta os espaços intermedios, e o \n da linha anterior *)
    Scanf.scanf "%c" (fun _ -> ());
    (* Lê o estado*)
    let tmp = Scanf.scanf " %d" (fun a -> a) in
    (* Actualiza a lista de estados finais*)
    l := (!l)@(tmp)::[]
  done;
  !l

let f = Scanf.scanf " %d" (fun a -> a)

let finais =
  let l = ref [] in
  (* Lê f numeros, os estados finais *)
  for i = 0 to f-1 do
    (* Descarta os espaços intermedios, e o \n da linha anterior *)
    Scanf.scanf "%c" (fun _ -> ());
    (* Lê o estado*)
    let tmp = Scanf.scanf " %d" (fun a -> a) in
    (* Actualiza a lista de estados finais*)
    l := tmp::(!l)
  done;
  !l

let m = Scanf.scanf " %d" (fun a -> a)


let list_input = ref []
let transicoes = 

  let t = Hashtbl.create m in
   (* Print do m*)
  for i = 0 to m-1 do
    
   (* Separa a chave do seu valor *)
    let k, v = Scanf.scanf " %d %c %d" (fun a b c-> (a, c), b) in
   (*guardar o k*)
    list_input := !list_input@[k,v];
    (* Adiciona o valor à chave *)
   (* Pode haver mais do que uma transição para o mesmo estado *)
    try
      let f = Hashtbl.find t k in
      f := !f@[v] 
    with Not_found -> Hashtbl.add t k (ref [v])
 
  done;
  t

 (* a ultima linha contem uma string representando a palavra t por reconhecer *)

  (*Separar o ultima em uma lista*)

let palavra = Scanf.scanf " %s" (fun a -> a)

let tam_ajuda= ref 0
let chars_of_string s =
  
  let l = ref [] in
  if s = "" then (l:=[' ']; tam_ajuda :=1)  else
  for p = 0 to String.length s - 1 do
    l := s.[p] :: !l
  done ;
  List.rev !l;;
let ultima = chars_of_string palavra


  (*Verifica se o automato é determinista se for da print a DFA se nao for dá print a NDFA*)
    (*DETERMINISTA
      *é só um estado inicial
      *nao pode ter transições _ para mais de um estado
      *so pode ter  transições com letras diferentes
*)
let determinista =
  let booleano = ref false in 
  if i != 1 then  booleano :=  true
  else
    for x = 1 to n do
      let aux = ref [] in 
      for y = 1 to n do
          (*percorre a hashtbl*)
        try
          let b =  (Hashtbl.find transicoes (x, y)) in
          if List.mem '_' !b then booleano := true else
            let boola = ref false in
            List.iter (fun a -> if List.mem a !aux then boola := true else aux := a::!aux) !b;
              (*adicionar ao aux se nao la estiver*)

              (*DEBUG
              List.iter (fun x -> print_char x) !b; print_newline();
              print_string (string_of_bool (!boola)); print_newline();
              DEBUG*)
          if !boola then (booleano :=  true; )
              (*print b*)
              (*Percorrer a hashtbl*)
        with Not_found -> ()
      done;

    done;
  if !booleano then print_endline "NDFA" else print_endline "DFA"
          

let rec percorre_automato ultima estado str = 
  let melhor = ref (false, "") in
  match ultima with
  | [] -> if List.mem estado finais then (true, str) else (false, "")
  | hd::tl -> 
      try
        let todos_os_possiveis = ref [] in 
        if !tam_ajuda = 1 then (todos_os_possiveis := (List.find_all (fun ((a,b), c) -> a = estado && (c = hd || (c ='_' ))) !list_input)) else(
        todos_os_possiveis := (List.find_all (fun ((a,b), c) -> a = estado && (c = hd || (c ='_' && a != b))) !list_input));
              
          
          print_endline str;
          print_string "estado: "; print_int estado; print_newline();
          print_string "HD: "; print_char hd; print_newline();
          print_string "TL: "; List.iter (fun x -> print_char x) tl; print_newline();
          List.iter (fun ((a,b), c) -> print_int (a); print_int (b); print_char c; print_endline "_ola1") !todos_os_possiveis;
          

        let tam = ref (List.length !todos_os_possiveis) in
        while !tam > 0 do
          let ((a,b), c) = List.hd !todos_os_possiveis in
          todos_os_possiveis := List.tl !todos_os_possiveis;
          let str_aux = ref "" in 
          if tl = [] && c != '_' && List.mem b finais then str_aux := str ^(string_of_int a)^ " " ^(string_of_int b) else(
            if !tam_ajuda = 1 && List.mem a finais && a = b then str_aux := str ^ (string_of_int a) else 
              if !tam_ajuda = 1 && List.mem b finais then str_aux := str ^(string_of_int a)^ " " ^(string_of_int b) else str_aux := str ^(string_of_int a)^ " ");
          if c = '_' then ( if !tam_ajuda = 1 then (if List.mem b finais then melhor := percorre_automato tl b !str_aux else melhor := percorre_automato ([hd]@tl) b !str_aux)   else(
            let bool, value = percorre_automato (hd::tl) b !str_aux in
            if bool then tam:=0; melhor:= (bool, value));)
          else (
            let bool, value = percorre_automato tl b !str_aux in
            if bool then tam:= 0;melhor:= (bool, value));
                             
          tam:= !tam -1;
                (*print_endline !str_aux;*)
        done; 
              (*DEBUG
              print_endline (string_of_bool (fst !melhor));
              print_endline (snd !melhor);
              DEBUG*)
        !melhor;
      with Not_found -> (false, "")
            
let com_inicial  = 
  let aux = ref 0 in
  let aux2 = ref iniciais in
  let str_final = ref "" in
  while i > !aux do
    aux := !aux + 1;
    let hd = List.hd !aux2 in
    aux2 := List.tl !aux2;
    let bool, str = percorre_automato ultima hd "" in
    if bool then (str_final:=("YES\n" ^str); aux:=i) else str_final:= "NO";
  done;
  !str_final
            (*Se o automato for NDFA procura a palavra pois NDFA tem varios caminhos possiveis*)

              
            

let () = print_endline com_inicial;
