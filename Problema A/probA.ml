(*  
Referências: 
  - Grupo do Teams, Enunciado do Trabalho.
  - http://www.di.ubi.pt/~desousa/TC/slides_automatos.pdf


  24/11/2022


Autores: 
  30646 - Luis Santos
  45842 - Diogo Santos
  
*)
open List
open Scanf
open Array

(*Lê o número de estados*)
let n = Scanf.scanf " %d" (fun a -> a)

(* Le o número de estados inicias*)
let i = Scanf.scanf " %d" (fun a -> a)

(* Le i estados e guarda os numa lista, sendo estes estados inicias*)
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

  (*Le o número de estados finais*)
let f = Scanf.scanf " %d" (fun a -> a)

(*Le f estados  e guarda os numa lista, sendo estes estados finais*)
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

(*Le o número de transições*)
let m = Scanf.scanf " %d" (fun a -> a)

(* Variavel que vai guardar as transições num formato ((a,c), b)
   Exemplo: ((1,2), a)*)
let list_input = ref []

(* Lê m linhas, cada uma com 3 elementos 
  Guarda os 3 elementos numa Hashtbl e numa lista!
  Na lista guarda ((a,c),b)
  Na hashtbl guarda de forma (a,c)-> b , pois é mais fácil de procurar para a função deterministica 
*)   

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

 (* a ultima linha contem uma string que o automato deve tentar reconhecer *)
let palavra = Scanf.scanf " %s" (fun a -> a)

(*Variavel para quando não existe palavra para reconhecer*)
let tam_ajuda= ref 0

(*Função que separa uma string para uma lista de caracteres*)
let chars_of_string s =
  
  let l = ref [] in
  if s = "" then (l:=[' ']; tam_ajuda :=1)  else
    for p = 0 to String.length s - 1 do
      l := s.[p] :: !l
    done ;
  List.rev !l;;
let ultima = chars_of_string palavra


  (*Verifica se o automato é determinista se for da print a DFA se nao for dá print a NDFA
    Determinista é:
      * é só um estado inicial
      * nao pode ter transições _ para mais de um estado
      * so pode ter transições com letras diferentes
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
            if !boola then (booleano :=  true; )
        with Not_found -> ()
      done;
    done;
  if !booleano then print_endline "NDFA" else print_endline "DFA"
          

 (*
    Recebe um estado e uma string
    Devolve um bool e a string
    Esta função é para quando o automato não é determinista, e se a palavra for aceite ele vai verificar se existe algum 
    caminho com epsilon que leva ao estado final
 *) 
let rec caminho_possivel estado str =
  let str_aux = ref str in
  let aux = ref false in
  let possiveis = ref (List.find_all (fun ((a,b), c) -> a = estado && c = '_') !list_input) in
  let tam_aux = ref (List.length !possiveis) in
  while !tam_aux > 0 do
    let ((a,b), c) = List.hd !possiveis in
    possiveis := List.tl !possiveis;
    tam_aux := !tam_aux - 1;
    if List.mem b finais then( str_aux:= !str_aux^(string_of_int b); aux:=true; tam_aux:= 0)else
      let naosei = caminho_possivel b (!str_aux ^(string_of_int b)^" ") in
      aux:=fst(naosei); str_aux := snd(naosei);
  done;
  (!aux,!str_aux)


(*
  Recebe uma lista com os caracteres da palavra para reconhecer, um estado e uma string
  Devolve um bool e uma string
   Se ultima tiver vazia, e o estado for final, devolve true e a string com o caminho percorrido se não devolve false e a string
   Se não estiver vazia, ele vai ver todos os caminhos possiveis:
      Se a palavra por reconhecer for "" (isto é tam_ajuda = 1) ele procura até os caminhos com epsilon para si, se a palavra não for "" ele procura os caminhos com epsilon e as letras para si e para os outros estados
    Depois percorre todos os caminhos possiveis:
      Verifica se estamos na uma ultima letra (!="_") e depois verificamos se o proximo estado não for final ele acaba este caminho se o estado seguint for final entao adiciona o estado onde está e o seguinte a string   
      Se houver outra letra da palavra ou só se houver transições epsilon então ele vai verificar se a palavra é vazia(tam_ajuda =  1 ) e se a é estado final e tem uma transição epsilon 
        para si, se for verdadeiro ele adiciona o estado onde a string caso de a palavra ser vazia mas pode ser reconhecida com um movimento para si
        ou então se a palavra for vazia e o estado atual tiver uma ligação epsilon para outro estado final então adiciona o estado onde está e o estado seguinte a string
        se nada disto acontecer adiciona o estado onde está a string      
      Se a transição for epsilon: e a palavra vazia e o estado seguinte final entao ele faz esse percurso se não for palavra continua chama novamente a função com as mesmas letras que faltam, o estado seguinte e a string
          Se a palavra não for vazia percorre para o estado seguinte sem gastar a letra e adiciona o estado onde está a string e verifica se esse caminho dá true se der acaba a pesquisa nos todos caminhos possiveis
      Se a transição for uma letra: verificamos se a proxima letra existe  se não existe vemos se o proximo estado é final se não for vai procurar um caminho por epsilons se nao encontrar devolve false e str
                                    se existir uma proxima letra percorremos para o proximo estado e adicionamos o estado onde estamos a string e chamamos a função com a proxima letra, o proximo estado e a string
      Ao final do while devolvemos o bool e a string
      *)                
let rec percorre_automato ultima estado str = 
  let melhor = ref (false, "1") in
  match ultima with
  | [] -> if List.mem estado finais then  (true, str) else (false, "2")
  | hd::tl -> 
      try
        let todos_os_possiveis = ref [] in 
        if !tam_ajuda = 1 then (todos_os_possiveis := (List.find_all (fun ((a,b), c) -> a = estado && (c = hd || (c ='_' ))) !list_input)) else(
          todos_os_possiveis := (List.find_all (fun ((a,b), c) -> a = estado && (c = hd || (c ='_' && a != b))) !list_input));
        let tam = ref (List.length !todos_os_possiveis) in
        while !tam > 0 do
          let ((a,b), c) = List.hd !todos_os_possiveis in
          todos_os_possiveis := List.tl !todos_os_possiveis;
          let str_aux = ref "" in 
          if tl = [] && c != '_' then(  if not (List.mem b finais) then ( melhor:= percorre_automato [] b (str ^(string_of_int a)^ " ")) else str_aux := str ^(string_of_int a)^ " " ^(string_of_int b) )
          else(
            if !tam_ajuda = 1 && List.mem a finais && a = b then str_aux := str ^ (string_of_int a) else 
            if !tam_ajuda = 1 && List.mem b finais then str_aux := str ^(string_of_int a)^ " " ^(string_of_int b) else str_aux := str ^(string_of_int a)^ " "
          );
          if c = '_' then ( if !tam_ajuda = 1 then (if List.mem b finais then melhor := percorre_automato tl b !str_aux else melhor := percorre_automato ([hd]@tl) b !str_aux)   
                            else(
                              let bool, value = percorre_automato (hd::tl) b !str_aux in
                              if bool then (tam:=0; melhor:= (bool, value))
                            );
                          )
          else (
            if tl = [] then (if List.mem b finais then melhor := percorre_automato tl b !str_aux else
                               (
                                 let bool, value = caminho_possivel b (str ^(string_of_int a)^ " " ^(string_of_int b)^ " ") in
                                 if bool then (tam:= 0;melhor:= (bool, value));

                               )) else (
                
              let bool, value = percorre_automato tl b !str_aux in
              if bool then (tam:= 0;melhor:= (bool, value))));
                             
          tam:= !tam -1;
        done; 
    
        !melhor;
      with Not_found -> (false, "3")
            

(*
   Percorre a lista dos automatos inicais e chama a funcao percorre_automato para cada um deles
   e se a palavra for aceite pelo automato imprime YES e o caminho percorrido se nao imprime NO
*)
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

              
            
(*MAIN*)
let () = print_endline com_inicial;
(*
  Indice:
  1º Exemplo de uma execução:
    1. Input
    2. Execução do Programa
    3. Ouput
  2º Exemplo de uma execução;
    1. Input
    2. Execução do Programa
    3. Ouput

  1º Exemplo de uma execução:
  
  1. Input 
    2
    1
    1
    1
    1
    3
    1 _ 1
    1 _ 2
    2 a 1
    b
  2. Execução do Programa
    n=2
    i=1
    f=1
    iniciais=[1]
    finais=[1]
    list_input=[((1, 1), '_'); ((1, 2), '_'); ((2, 1), 'a')]
    transicoes (hashtbl) = 
    (
      (1, 1) = ['_']
      (1, 2) = ['_']
      (2, 1) = ['a']
    )
    ultima=['b']
    deterministico=false (porque temos transições epsilon)
    Na percorre_automato ultima 1 "" 
      Vai só aparecer um caminho possivel sendo ele [((1, 1), '_')] mas como este tem transições epsilons para ele mesmo então vai dar false
    com_inicial = "NO" (porque não existe caminho que aceite a palavra b)

  3. Output
    NDFA
    NO
  2º Exemplo de uma execução:

  1. Input 
    2
    1
    1
    1
    1
    3
    1 a 2
    1 b 2
    2 b 1
    ab
  2. Execução do Programa
    n=2
    i=1
    f=1
    iniciais=[1]
    finais=[1]
    list_input=[((1, 1), 'a'); ((1, 2), 'b'); ((2, 1), 'a')]
    transicoes (hashtbl) = 
    (
      (1, 2) = ['a']
      (1, 2) = ['b']
      (2, 1) = ['a']
    )
    ultima=['a','b']
    deterministico=true (porque não tem transições epsilon, só tem um estado inicial e não tem transições com o mesmo valor de um estado para outro)
    Na percorre_automato ultima 1 "" :
      hd = 'a'
      tl = ['b']
      estado = 1
      str = ""
      tam = 2
      todos_os_possiveis = [((1, 2), 'a'); ((1, 2), 'b')]
      str_aux = ""
      então como hd = 'a' ele vai optar por este 
      hd = 'b'
      tl = []
      estado = 2
      str = "1 "
      tam = 1
      todos_os_possiveis = [((2, 1), 'a')]
      str_aux = "1 "
      então como hd = 'b' ele vai optar por este e como tl = [] e o estado é final então str_aux = "1 2 1" onde vai returnar (true, "1 2 1")
    com_inicial = 
    "YES"
    "1 2 1"

  3. Output
    DFA
    YES
    1 2 1

*)