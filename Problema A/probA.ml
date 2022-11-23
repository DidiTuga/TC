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
  let ultima = Scanf.scanf " %s" (fun a -> a)
  (*Separar o ultima em uma lista*)
  let chars_of_string s =
    let l = ref [] in
      for p = 0 to String.length s - 1 do
        l := s.[p] :: !l
      done;
      List.rev !l;;
  let ultima = chars_of_string ultima
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
              let b = Hashtbl.find transicoes (x, y) in
              if !b = ['_'] then booleano := true else  
              (*adicionar ao aux se nao la estiver*)
                if not (List.mem !b !aux) then aux := !b::!aux
                else booleano :=  true
              (*print b*)
              (*Percorrer a hashtbl*)
            with Not_found -> ()

            done
          done;

      if !booleano then print_endline "NDFA" else print_endline "DFA"
          

        let rec percorre_automato_NDFA ultima estado str = 
          let melhor = ref (false, "") in
          match ultima with
          | [] -> if List.mem estado finais then (true, str) else (false, "")
          | hd::tl -> 
              try
              let todos_os_possiveis = ref(List.find_all (fun ((a,b), c) -> a = estado && (c = hd || c ='_')) !list_input)in
              (*DEBUG
              if str = "1 2 1 3 " then (
                print_endline str;
              print_endline (string_of_int estado);
              print_endline (Char.escaped hd);
              List.iter (fun x -> print_char x; print_endline"_ola") ultima;

                List.iter (fun ((a,b), c) -> print_int (a); print_int (b); print_char c; print_endline "_ola1") !todos_os_possiveis);
                DEBUG*)
              let tam = ref (List.length !todos_os_possiveis) in
              while !tam > 0 do
                let ((a,b), c) = List.hd !todos_os_possiveis in
                todos_os_possiveis := List.tl !todos_os_possiveis;
                let str_aux = ref "" in 
                if tl = [] then str_aux := str ^(string_of_int a)^ " " ^(string_of_int b) else str_aux := str ^ (string_of_int a)^ " ";
                if c = '_' then (
                  let bool, value = percorre_automato_NDFA (hd::tl) b !str_aux in
                  if bool then tam:=0; melhor:= (bool, value);)
                            else (
                  let bool, value = percorre_automato_NDFA tl b !str_aux in
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
              let bool, str = percorre_automato_NDFA ultima hd "" in
              if bool then (str_final:=("YES\n" ^str); aux:=i) else str_final:= "NO";
            done;
            !str_final
            (*Se o automato for NDFA procura a palavra pois NDFA tem varios caminhos possiveis*)

              
            

    let () = print_endline com_inicial;
