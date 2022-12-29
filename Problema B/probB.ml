(*
Autores: 
  30646 - Luis Santos
  45842 - Diogo Santos
  
*)

(*Algoritmo CYK (de Cocke, Younger e Kasami)*)
(*Input *)

(*Ler palavra para reconhecer *)
let palavra = read_line ()

(*Ler quantas transições temos*)
let num_transicoes = read_int ()

(*Função para ler uma transição e devolve os caracteres, o primeiro é de onde vem para onde vai S -> a = (S,a)*)
let lerLinha = 
  let list = ref [] in
  for i = 1 to num_transicoes do
    let linha = read_line () in
    if String.length linha == 6 then list:= !list @ [(linha.[0], linha.[5], ' ')]
    else list:= !list @ [(linha.[0], linha.[5], linha.[7])]
  done;
  !list


(*Lista onde vai guardar todas as transiçoes*)
let list_trans = lerLinha


let explode_string s = List.init (String.length s) (String.get s)



(*Função para verificar se uma letra está na lista*)

(*cyk é a matriz que vai guardar os resultados*)
let cyk = 
  (*
  DEBUG
  List.iter (fun (a,b,c) -> print_endline (String .make 1 a ^ String .make 1 b ^ String .make 1 c )) list_trans;
  *)
  (*tamanho da palavra*)
  let m = String.length palavra in
  (*x y valor para inicial*)
  let matrix = Array.make_matrix (m) (m+1) [] in
  (*inicializar com a ultima camada a 0*)
  for i = 0 to m-1 do
    matrix.(i).(m) <- [palavra.[i]]
  done;
  (*Preencher a matriz*)
  let novas_transicoes = ref [] in
  for j = m-1 downto 0 do
    for i = 0 to j do
      (*numero de caracteres a ir buscar *)
      let num = m - j in
      let string = String.sub palavra i num in
      let list_string = explode_string string in
      (*print_endline string;*)
      (*verificar se a string pertence a L(G)*)
      (*Ir percorrer caracter a caracter*)
      if List.length list_string == 1 
      then(
        (*Verificar se o caracter está na lista*)
        let list = List.find_all (fun (a,b,c) -> b = List.hd list_string || c = List.hd list_string) list_trans in
        (*Se estiver, adicionar ao array*)
        if list <> [] then
          matrix.(i).(j) <- matrix.(i).(j) @ (List.map (fun (a,b,c) -> a) list);
          novas_transicoes := !novas_transicoes @ List.map (fun (a,b,c) -> (String.make 1 a, String.make 1 b, String.make 1 c)) list
      )else(
        (*Percorrer a string exemplo aa fica a,a ; aba -> a,ba e ab,a*)
        for k = 0 to num-2 do
          let cor_string_1 = String.sub string 0 (k+1) in
          let cor_string_2 = String.sub string (k+1) (num-k-1) in
          (*DEBUG
          print_endline ("cor_1" ^ cor_string_1);
          print_endline ("cor_2" ^ cor_string_2);
          *)
          let list = List.map (fun (a,b,c) -> (a)) (List.find_all (fun (a,b,c) -> b = cor_string_1 || c = cor_string_1) !novas_transicoes) in 
          let list_2 = List.map (fun (a,b,c) -> (a)) (List.find_all (fun (a,b,c) -> b = cor_string_2 || c = cor_string_2) !novas_transicoes) in 
          if list <> [] && list_2 <> [] then
            (*junta os a das listas *)
            let nova_lista = List.concat(List.map (fun x -> List.map (fun y -> (x,y)) list_2) list) in
            (*Procurar os que tem x,y*)
            let nova_lista = List.concat ( List.map  (fun (x,y)  ->  List.find_all( fun (a,b,c) ->  (String.make 1 b)=x && y =(String.make 1 c)) list_trans )  nova_lista) in
            

            (*Retirar os repetidos e meter por ordem alphabetica*)

            let nova_lista = List.sort_uniq (fun (a,b,c) (d,e,f) -> compare a d) nova_lista in
            (*DEBUG
            List.iter (fun (a,b,c) -> print_endline (String.make 1 a)) nova_lista;
            *)
            
          (*Se estiver, adicionar ao array*)
            matrix.(i).(j) <- matrix.(i).(j) @ (List.map (fun (a,b,c) -> a) nova_lista);
            novas_transicoes := !novas_transicoes @ List.map (fun (a,b,c) -> (String.make 1 a, cor_string_1^cor_string_2, "")) nova_lista

          

        done
      )
    done;
  done;
  (*Se exister um S na primeira posição da matriz da print a YES se não a NO*)
  if List.mem 'S' matrix.(0).(0) then print_endline "YES" else print_endline "NO";
  
  (**Print da matriz que guarda os resultados em lista de chars*)
  for i = 0 to m do
    for j = 0 to m-1 do
      Printf.printf "%s\t\t" (String.concat " " (List.map (fun x -> String.make 1 x) (List.sort_uniq (fun (a) (d) -> compare a d) matrix.(j).(i))))
    done;
    Printf.printf "\n"
  done;;
  
  
  

