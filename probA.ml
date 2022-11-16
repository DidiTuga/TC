open List
open Scanf
open Array

let n = Scanf.scanf " %d" (fun a -> a)

let i = Scanf.scanf " %d" (fun a -> a)

let f = Scanf.scanf " %d" (fun a -> a)

let finais =
  let l = ref [] in
  (* Lê f numeros, os estados finais *)
  for i = 0 to f-1 do
    (* Descarta os espaços intermedios, e o \n da linha anterior *)
    Scanf.scanf "%c" (fun _ -> ());
    (* Lê o estado*)
    let tmp = Scanf.scanf "%d" (fun a -> a) in
    (* Actualiza a lista de estados finais*)
    l := tmp::(!l)
  done;
  !l

let m = Scanf.scanf " %d" (fun a -> a)

(*uma linha com o numero m de transicoes*)
let transicao = 
  let matrix = make_matrix n 256 0 in

(*le as transicoes*)
for i = 0 to m-1 do
  (* Descarta os espaços intermedios, e o \n da linha anterior *)
  Scanf.scanf "%c" (fun _ -> ());
  (* Lê o estado*)
  let tmp = Scanf.scanf "%d" (fun a -> a) in
  (* Descarta os espaços intermedios, e o \n da linha anterior *)
  Scanf.scanf "%c" (fun _ -> ());
  (* Lê o simbolo*)
  let tmp2 = Scanf.scanf "%c" (fun a -> a) in
  (* Descarta os espaços intermedios, e o \n da linha anterior *)
  Scanf.scanf "%c" (fun _ -> ());
  (* Lê o estado*)
  let tmp3 = Scanf.scanf "%d" (fun a -> a) in
  (* Actualiza a matriz*)
  matrix.(tmp).(int_of_char tmp2) <- tmp3
done
  





let transicoes = 
  let t = Hashtbl.create m in
  for i = 0 to m-1 do
   (* Separa a chave do seu valor *)
   let k, v = Scanf.scanf " %d %c %d" (fun a b c-> (a, c), b) in
   (* Pode haver mais do que uma transição para o mesmo estado *)
   try
     let f = Hashtbl.find t k in
     f := !f@[v] 
   with Not_found -> Hashtbl.add t k (ref [v])
 
  done;
  t
 (* a ultima linha contem uma string representando a palavra t por reconhecer *)
  let ultima = Scanf.scanf " %s" (fun a -> a)

