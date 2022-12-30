(*  Algoritmo CYK (de Cocke, Younger e Kasami)
  Referências: 
  - Grupo do Teams, Enunciado do Trabalho.

  30/12/2022

  Autores: 
  30646 - Luis Santos
  45842 - Diogo Santos
*)
open List 

(*Ler palavra para reconhecer *)
let palavra = read_line ()

(*Ler quantas transições temos*)
let num_transicoes = read_int ()

(* Sabendo que são no maximo 3 caracteres, a função vai ler a linha e devolver uma lista com os caracteres*)
let lerLinha = 
  let list = ref [] in
  for i = 1 to num_transicoes do
    let linha = read_line () in
    let linha = String.split_on_char '>' linha in
    let linha = String.concat "" linha in
    let linha = String.split_on_char '-' linha in
    let linha = String.concat "" linha in
    let linha = String.split_on_char ' ' linha in
    let linha = String.concat "" linha in
    if String.length linha < 3 then list:= !list @ [((String.make 1 linha.[0]), (String.make 1 linha.[1]), " ")]
    else list:= !list @ [((String.make 1 linha.[0]), (String.make 1 linha.[1]), (String.make 1 linha.[2]))]
  done;
  !list
  
(*Lista onde vai guardar todas as transiçoes*)
let list_trans = lerLinha

(*Função que vai percorrer a matriz e verificar se a palavra pertence a L(G) *)
let cyk = 
  (*tamanho da palavra*)
  let m = String.length palavra in
  (*x y valor para inicial*)
  let matrix = Array.make_matrix (m) (m+1) [] in
  (*inicializar com a ultima camada a 0*)
  for i = 0 to m-1 do
    matrix.(i).(m) <- [String.make 1 palavra.[i]]
  done;
  (*Variavel que vai guardar as novas transições*)
  let novas_transicoes = ref [] in
  for j = m-1 downto 0 do (*j é colunas y*)
    for i = 0 to j do (*i é as linhas x*)
      let num = m - j in
      let string = String.sub palavra i num in
      (*Para um caracater vai verificar se está na lista e se estiver adiciona na matriz e nas novas transições*)
      if String.length string = 1 then(
        (*Verificar se o caracter está na lista*)
        let list = find_all (fun (a,b,c) -> b = string || c = string) list_trans in
        (*Se estiver, adicionar a matriz*)
        if length list >0 then
          matrix.(i).(j) <- matrix.(i).(j) @ (map (fun (a,b,c) -> a) list);
          novas_transicoes := !novas_transicoes @ map (fun (a,b,c) -> (a, b, c)) list
      )else(
        (*Percorrer a string exemplo: aba -> a,ba e ab,a*)
        for k = 0 to num-2 do
          let cor_string_1 = String.sub string 0 (k+1) in
          let cor_string_2 = String.sub string (k+1) (num-k-1) in
          (*Verificar se as 2 string estão na lista*)
          let list_1 = map (fun (a,b,c) -> (a)) (find_all (fun (a,b,c) -> b = cor_string_1 || c = cor_string_1) !novas_transicoes) in 
          let list_2 = map (fun (a,b,c) -> (a)) (find_all (fun (a,b,c) -> b = cor_string_2 || c = cor_string_2) !novas_transicoes) in 
          if length list_1 > 0 && length list_2 > 0 then(
            (*junta os "a" das listas *)
            let nova_lista = concat(map (fun x -> map (fun y -> (x,y)) list_2) list_1) in
            (*Procurar os que tem x,y*)
            let nova_lista = concat ( map  (fun (x,y)  ->  find_all( fun (a,b,c) ->  b=x && y =c) list_trans )  nova_lista) in
            (*Retirar os repetidos e meter por ordem alphabetica*)
            let nova_lista = sort_uniq compare nova_lista in
          (*adiciona na matriz*)
            matrix.(i).(j) <- matrix.(i).(j) @ (map (fun (a,b,c) -> a) nova_lista);
            let nova_lista = map (fun (a,b,c) -> (a, cor_string_1^cor_string_2, "")) nova_lista in
            (* se a nova_lista nao tiver nas novas transições adiciona*)
            if for_all (fun (a,b,c) -> not (mem (a,b,c) !novas_transicoes)) nova_lista then novas_transicoes := !novas_transicoes @ nova_lista;)
        done
      )
    done;
  done;
  (*Se exister um S na primeira posição da matriz da print a YES se não a NO*)
  if mem "S" matrix.(0).(0) then print_endline "YES" else print_endline "NO";
  (**Print da matriz que guarda os resultados em lista de chars*)
  for i = 0 to m do
    for j = 0 to m-1 do
      Printf.printf "%s\t\t" (String.concat " " (map (fun x -> x) (sort (fun (a) (d) -> compare a d) (sort_uniq compare matrix.(j).(i)))))
    done;
    Printf.printf "\n"
  done;;

  (*
  Indice:
    1. Input
    2. Execução do Programa
    3. Ouput 
    
  1. Input
    aabaa
    4
    S -> A B
    S -> b
    A -> a 
    B -> S A
  
  2. Execução do Programa
  palavra = aabaa
  num_transicoes = 4
  list_trans = [("S", "A", "B"); ("S", "b", ""); ("A", "a", " "); ("B", "S", "A")]
  cyk =
  S										novas_transicoes = [("A", "a", ""); ("S", "b", ""); ("B", "ba", ""); ("S", "aba", ""); ("B", "abaa", ""), ("S", "aabaa", "")]
      B								novas_transicoes = [("A", "a", ""); ("S", "b", ""); ("B", "ba", ""); ("S", "aba", ""); ("B", "abaa", "")]
      S								novas_transicoes = [("A", "a", ""); ("S", "b", ""); ("B", "ba", ""); ("S", "aba", "")]
          B						novas_transicoes = [("A", "a", ""); ("S", "b", ""); ("B", "ba", "")]
  A		A		S		A		A		novas_transicoes = [("A", "a", ""); ("S", "b", "") ]
  a		a		b		a		a		
  3. Output
  YES
  S										
      B								
      S								
          B						
  A		A		S		A		A		
  a		a		b		a		a		
  *)