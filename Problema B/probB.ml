(*
Autores: 
  30646 - Luis Santos
  45842 - Diogo Santos
  
*)

(*Algoritmo CYK (de Cocke, Younger e Kasami)*)
  
  (*
    Entrada: 
      - Gramática G = (N, T, P, S)
      - cadeia w
    Saída: 
      - se w pertence a L(G) ou não
  *)
    
    (*
      Função CYK(G, w)
        - para todo i = 1, 2, ..., n faça
            - para todo A pertencente a N faça
                - M[i, i, A] := 0
        - para todo i = 1, 2, ..., n faça
            - para todo A pertencente a N faça
                - para todo B pertencente a P faça
                    - se B -> aA é uma regra de produção de G faça
                        - M[i, i, A] := M[i, i, A] + 1
        - para todo l = 2, 3, ..., n faça
            - para todo i = 1, 2, ..., n - l + 1 faça
                - para todo j = i + 1, i + 2, ..., i + l - 1 faça
                    - para todo A pertencente a N faça
                        - para todo B pertencente a P faça
                            - se B -> CD é uma regra de produção de G faça
                                - M[i, j, A] := M[i, j, A] + M[i, j - 1, C] * M[j, i + l - 1, D]
        - se M[1, n, S] > 0 então
            - retorne verdadeiro
        - senão
            - retorne falso
      Fim da função CYK(G, w)
    
    type regra_producao = {esquerda: string; direita: string}
    type nao_terminais = (int, string) Hashtbl.t
    type terminais = (int, string) Hashtbl.t
    type regras_producao = (int, regra_producao) Hashtbl.t
    type gramatica = {nao_terminais: nao_terminais; terminais: terminais; regras_producao: regras_producao; inicial: string}





 
        


    let cyk gramatica cadeia =
      let n = String.length cadeia in
      let m = Array.make_matrix (n + 1) (n + 1) (Hashtbl.create 0) in
      for i = 1 to n do
        for a = 0 to (Hashtbl.length gramatica.nao_terminais) - 1 do
          Hashtbl.add m.(i).(i) (Hashtbl.find gramatica.nao_terminais a) 0
        done
      done;
      print_endline"ERR";
      for i = 1 to n do
        for a = 0 to (Hashtbl.length gramatica.nao_terminais) - 1 do
          for b = 0 to (Hashtbl.length gramatica.regras_producao) - 1 do
            let regra = Hashtbl.find gramatica.regras_producao b in
            if regra.esquerda = (Hashtbl.find gramatica.nao_terminais a) && regra.direita = (String.make 1 cadeia.[i - 1]) then
              Hashtbl.replace m.(i).(i) (Hashtbl.find gramatica.nao_terminais a) ((Hashtbl.find m.(i).(i) (Hashtbl.find gramatica.nao_terminais a)) + 1)
          done
        done
      done;
      print_endline"ERR1";
      for l = 2 to n do
        for i = 1 to n - l + 1 do
          for j = i + 1 to i + l - 1 do
            for a = 0 to (Hashtbl.length gramatica.nao_terminais) - 1 do
              for b = 0 to (Hashtbl.length gramatica.regras_producao) - 1 do
                let regra = Hashtbl.find gramatica.regras_producao b in
                print_endline (string_of_int (b));
                if regra.esquerda = (Hashtbl.find gramatica.nao_terminais a) then
                  Hashtbl.replace m.(i).(i + l - 1) (Hashtbl.find gramatica.nao_terminais a) ((Hashtbl.find m.(i).(i + l - 1) (Hashtbl.find gramatica.nao_terminais a))
                  + ((Hashtbl.find m.(i).(j - 1) (String.make 1 regra.direita.[0])) * (Hashtbl.find m.(j).(i + l - 1) (String.make 1 regra.direita.[1]))))
              done
            done
          done
        done
      done;
      print_endline"ERR2";
      if (Hashtbl.find m.(1).(n) gramatica.inicial) > 0 then
        true
      else
        false

        let () = 
        let gramatica =  {nao_terminais = Hashtbl.create 0; terminais = Hashtbl.create 0; regras_producao = Hashtbl.create 0; inicial = ""} in 
        Hashtbl.add gramatica.nao_terminais 0 "S";
        Hashtbl.add gramatica.nao_terminais 1 "A";
        Hashtbl.add gramatica.nao_terminais 2 "B";
        Hashtbl.add gramatica.nao_terminais 3 "C";
        Hashtbl.add gramatica.terminais 0 "a";
        Hashtbl.add gramatica.terminais 1 "b";
        Hashtbl.add gramatica.terminais 2 "c";
        Hashtbl.add gramatica.regras_producao 0 {esquerda = "S"; direita = "AB"};
        Hashtbl.add gramatica.regras_producao 1 {esquerda = "S"; direita = "BC"};
        Hashtbl.add gramatica.regras_producao 2 {esquerda = "A"; direita = "aA"};
        Hashtbl.add gramatica.regras_producao 3 {esquerda = "A"; direita = "b"};
        Hashtbl.add gramatica.regras_producao 4 {esquerda = "B"; direita = "bB"};
        Hashtbl.add gramatica.regras_producao 5 {esquerda = "B"; direita = "c"};
        Hashtbl.add gramatica.regras_producao 6 {esquerda = "C"; direita = "cC"};
        Hashtbl.add gramatica.regras_producao 7 {esquerda = "C"; direita = "a"};
        print_endline( string_of_bool( cyk gramatica "abcc"));;
(*
  Exemplo de uso:
    let gramatica = {
      nao_terminais = Hashtbl.create 0;
      terminais = Hashtbl.create 0;
      regras_producao = Hashtbl.create 0;
      inicial = "S"
    } in
    Hashtbl.add gramatica.nao_terminais 0 "S";
    Hashtbl.add gramatica.nao_terminais 1 "A";
    Hashtbl.add gramatica.nao_terminais 2 "B";
    Hashtbl.add gramatica.nao_terminais 3 "C";
    Hashtbl.add gramatica.terminais 0 "a";
    Hashtbl.add gramatica.terminais 1 "b";
    Hashtbl.add gramatica.terminais 2 "c";
    Hashtbl.add gramatica.regras_producao 0 {esquerda = "S"; direita = "AB"};
    Hashtbl.add gramatica.regras_producao 1 {esquerda = "S"; direita = "BC"};
    Hashtbl.add gramatica.regras_producao 2 {esquerda = "A"; direita = "aA"};
    Hashtbl.add gramatica.regras_producao 3 {esquerda = "A"; direita = "b"};
    Hashtbl.add gramatica.regras_producao 4 {esquerda = "B"; direita = "bB"};
    Hashtbl.add gramatica.regras_producao 5 {esquerda = "B"; direita = "c"};
    Hashtbl.add gramatica.regras_producao 6 {esquerda = "C"; direita = "cC"};
    Hashtbl.add gramatica.regras_producao 7 {esquerda = "C"; direita = "a"};
    cyk gramatica "abcc"
*)*)
(*Input *)

let palavra = Scanf.scanf " %s" (fun a -> a)

let num_transicoes = Scanf.scanf " %d" (fun a -> a)



let list_trans = List.init num_transicoes (fun _ -> Scanf.scanf " %s -> %s\n" (fun a b -> (a, b)))

(*Recebe uma string e separa por espaços e devolve em (a , b)*)



let () = 
(*Print list_trans*)
let a, b = List.hd list_trans in
print_endline b;
List.iter (fun (a, b) -> print_endline ( a ^ " -> " ^  b  )) (list_trans)