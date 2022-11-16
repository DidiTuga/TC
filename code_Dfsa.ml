open List
open Str
open Scanf
open Printf

(**
ExecuÃ§Ã£o de autÃ³matos deterministas

*)




(**
O tipo simbolo representa o tipo das letras (o alfabeto - presentes nas fitas mas tambÃ©m nas transiÃ§Ãµes). Aqui fixamos o tipo char como sendo o alfabeto usado nestes autÃ³matos.

*)


type simbolo = char 


(** a fita de entrada Ã© simplesmente uma lista de simbolos *)

type fita =  simbolo list

(**
Escolhemos representar os estados por inteiros. Em particular tentaremos respeitar o invariante seguinte sobre a representaÃ§Ã£o dos estados: Se houver n estados entÃ£o estes sÃ£o 0 1 2 3 .. n-1.

*)


type estado = int 

(**
As transiÃ§Ãµes q1 --a--> q2 sÃ£o representadas como ((q1,a),q2) ao detrimento da representaÃ§Ã£o mais natural (q1,a,q2).

Esta pequena nuance permite uma pesquisa melhorada de uma transiÃ§Ã£o no conjunto das possÃ­veis transiÃ§Ãµes (de tipo hash table, em que a chave Ã© (q1,a) e o conteudo Ã© estado destino q2)

*)


type transicao =  ((estado*simbolo)*estado)

(**
Neste cenÃ¡rio, um autÃ³mato (ou mÃ¡quina) Ã© dado pela relaÃ§Ã£o de transiÃ§Ã£o (a listas de adjacÃªncia, ou seja a lista das transiÃ§Ãµes), *o* estado inicial e o conjunto dos estados finais. Sendo que o alfabeto e conjunto dos estados se deduz automaticamente dos dados anteriores. *)


type maquina =  (transicao list * estado * estado list)

(**
As configuraÃ§Ãµes da mÃ¡quina (determinista), aqui designada de memÃ³ria, Ã© simplesmente o par *do* estado actualmente activo (onde a mÃ¡quina se encontra no momento a execuÃ§Ã£o) e o buffer que resta ainda por processar.

*)

 
type memoria = (estado * fita)

(** uma excepÃ§Ã£o para assinalar o fim de uma execuÃ§Ã£o *)

exception FIM of memoria



(**
next calcula o prÃ³ximo estado, ou seja o estado q destino da transiÃ§Ã£o esta---simb--->q, se esta transiÃ§Ã£o existir. SenÃ£o (i.e. a excepÃ§Ã£o Not_found foi lanÃ§ada) esta situaÃ§Ã£o configura o fim da execuÃ§Ã£o (situaÃ§Ã£o processada pela funÃ§Ã£o step).

*)


let next (simb:simbolo)  (aqui:estado) (maq:maquina)= 
    let transicoes,b,c = maq in
    (assoc (aqui,simb) transicoes)




(** step realiza um passo de execuÃ§Ã£o do autÃ³mato maq a partir da configuraÃ§Ã£o memo. *)

let step (memo:memoria) (maq:maquina) = 
  let (aqui, restante) = memo in
  
  	(** se o buffer de entrada for vazio, entÃ£o acabou, senÃ£o tratamos do primeiro caracter do buffer. Ou seja, vamos ver que novo estado atingimos com este caracter a partir do estado actualmente activo (onde a execuÃ§Ã£o actualmente se encontra, o estado aqui). Chamanos aqui a funÃ§Ã£o next que trata deste cÃ¡lculo. *)

  match restante with
      [] ->  raise (FIM memo)
    | el::li ->
       try  
         (((next el aqui maq),(li:fita)) : memoria) 
       with Not_found -> raise (FIM memo)

(** is_accepted Ã© um predicado que detecta se uma configuraÃ§Ã£o memo da execuÃ§Ã£o do autÃ³mato maq prefigura a aceitaÃ§Ã£o. Ou seja o buffer de entrada encontra-se vazio e hÃ¡ pelo menos um estado final na configuraÃ§Ã£o *)

let is_accepted (memo:memoria) (maq:maquina) =
  let (aqui, restante) = memo in
  let (trans,init,accept)= maq in
  (mem aqui accept)&&(restante=[])

(** funÃ§Ãµes utilitÃ¡rias simples*)

let em_par a b c =  ((a, b),c);;

let char_of_string s = s.[0];;

(** LÃª no formato texto a palavra por reconhecer e o autÃ³mato por executar. A leitura devolve as estruturas correspondentes. *)

let leitura () =
  let dados = map (fun x -> char_of_string x) 
   (Str.split (Str.regexp "[ \t]+")  (read_line ())) in
  let initl = read_int () in
  let finl = map (fun x -> int_of_string x) 
    (Str.split (Str.regexp "[ \t]+")  (read_line ()))  in
  let input = ref [] in
    try
      while (true) do
        input:= (scanf " %d %c %d " em_par)::!input
      done; (dados,(!input,initl,finl))
    with _ -> ((dados:fita),((!input,initl,finl):maquina));;


(** a funÃ§Ã£o print_output analisa a configuraÃ§Ã£o final e imprime na saÃ­da standard o veredicto. *)

let print_output (memo:memoria) (maq:maquina)= 
    if (is_accepted memo maq) 
    then printf "YES\n"
    else printf"NO\n"


let main () =
  let dados,(maq:maquina) = leitura () in
  let (a,b,c) = maq in 
    try
      let memor = ref (b,dados)  in
        
        	(** Enquanto houver passos de execuÃ§Ã£o por realizar, executar. A excepÃ§Ã£o FIM Ã© lanÃ§ada para assinalaro fim da execuÃ§Ã£o. *)

      while true do
        memor := (step !memor maq)
      done
    with
        FIM x -> print_output x maq
;;

main ();;

(** exemplo de entrada: 
a a b a 0 1 2 0 a 0 0 b 1 0 a 3 1 a 2 2 a 3 3 a 1 3 a 2
*)

