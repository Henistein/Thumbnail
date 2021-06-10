(* Programação Funcional - Problema A
 * 43297 - Miguel Manteigueiro
 * 43931 - Henrique Jesus
 * last modified: 27 may 2021 
 * ----------------------------
 * Fontes utilizadas para a realização do trabalho:
 * [A] - Slides disponibilizados pelo Professor, na página da cadeira       {http://www.di.ubi.pt/~desousa/PF/pf.html}
 * [B] - Tutoriais disponibilizados na própria página da linguagem OCaml    {https://ocaml.org/learn/tutorials/} 
 * [C] - Documentação da linguagem OCaml                                    {https://ocaml.org/docs/}
 * [D] - Esclarecimento de dúvidas pela comunidade através de IRC chat      {https://webchat.freenode.net/, chat #ocaml}
 *)


(* Estruturas auxiliares 
 * color - cor de uma folha
 * image - árvore em si
 *)
type color = W | B (* W : White , B : Black *)
type image = L of color * int (* leaf of one color *)
           | N of image * image * image * image (* node with four children *)

(* Função auxiliar para ler do ficheiro *)
let read () = 
  try Some(read_line())
  with End_of_file -> None

(* Leitura do ficheiro para uma lista, com recurso à função acima *)
let rec build_list acc =
  match read () with
  | Some(line) -> build_list (line :: acc)
  | None -> List.rev acc


(* Remover os primeiros dois elementos e o último elemento da lista (apenas nos interessa a matriz) *)
let filter_list m =
  let m1 = function (h1 :: h2 :: t) -> t | _ -> failwith "erro" in
  let m2 = function (h :: t) -> t | _ -> failwith "erro" in
  let m3 = function l -> List.rev (m2 (List.rev l)) in
  m3(m1(m))

(* Converter a lista de strings para uma lista de inteiros, para trabalhar nesta mesmo *)
let convert_to_int m =
	let c1 = fun l -> List.map (String.split_on_char ' ') l in
	let rec c2 = function
	 | [] -> []                                               
	 | h :: t -> (List.map (int_of_string) h) :: (c2 t) in
	c2(c1(m))

(* Função que divide uma lista ao meio pela esquerda *)
let split_left m =
  let rec take n = function
   | h :: t when n > 0 -> h :: take (n-1) t
   | _ -> []
  in take (List.length m / 2) m

(* Função que divide uma lista ao meio pela direita *)
let split_right m =
  let rec take n = function 
   | h :: t when n > 0 -> h :: take (n-1) t
   | _ -> []
  in List.rev (take (List.length m / 2) (List.rev m))

(* Função que divide a lista em quadrantes, dado uma flag. Vai formar os nossos quadrantes NW, NE, SW, SE *)
let slice lst flag = 
  match flag with
  | "NW" -> List.map (split_left) (split_left lst)
  | "NE" -> List.map (split_right) (split_left lst)
  | "SW" -> List.map (split_left) (split_right lst)
  | "SE" -> List.map (split_right) (split_right lst)
  | _ -> failwith "erro"

(* Função auxiliar que converte uma folha num bit *)
let leaf_bit = function W -> 0 | B -> 1

(* Função auxiiar que converte um bit numa folha *)
let bit_leaf = function 0 -> W | 1 -> B | _ -> failwith "erro"
(* Função que converte uma lista de bit numa folha *)
let bit_leaf_list = function [x] -> bit_leaf x | _ -> failwith "erro"
(* Função auxiliar que retorna 1 caso a folha seja branca e -1 caso a folha seja preta *)
let aux = function [x] when (x = 0) -> (1) | _ -> (-1)

(* Verifica se todos os elementos da lista possuem o mesmo bit (cor) *)
let same_color c lst =
  let rec single_list c = function
     | [] -> true
     | h :: t -> (h = c) && (single_list c t) in
  (List.mem false (List.map (single_list c) lst)) = false

(* Função que determina o tamanho de uma list list *)
let rec ll_length = function
  | [] -> 0
  | h :: t -> (List.length h) + ll_length t

(* Função que constrói a árvore a partir da lista *)
let rec tree = function
  | [x] -> L((bit_leaf_list x), (aux x))
  | rest when (same_color 1 rest) = true -> L(B, (-1)*(ll_length rest))
  | rest when (same_color 0 rest) = true -> L(W, (ll_length rest))
  | rest -> N((tree (slice rest "NW")), (tree (slice rest "NE")), (tree (slice rest "SW")), (tree (slice rest "SE")))

(* Função que dadas 4 folhas, devolve a folha resultante (preto ou branco) *)
let single_leaf a b c d =
  let res = function
  | L(a, b) -> b
  | _ -> failwith "erro" in
  let final_leaf = function
    | x when (x <= 0) -> L(B, x)
    | x when (x > 0) -> L(W, x)
    | _ -> failwith "erro" in
    final_leaf ((res a) + (res b) + (res c) + (res d))

(* Função que desce recursivamente a árvore até encontrar uma folha *)
let rec go_down = function
	| L(a, b) -> L(a, b)
	| N(a, b, c, d) -> single_leaf (go_down a) (go_down b) (go_down c) (go_down d)

(* Função que dado uma camada, faz o corte da árvore *)
let rec cut_tree node goal inc =
	match node with
	| L(a, b) -> L(a, b)
	| N(a, b, c, d) when (goal = inc) -> single_leaf (go_down a) (go_down b) (go_down c) (go_down d)
	| N(a, b, c, d) when (goal <> inc) -> N((cut_tree a goal (inc+1)), (cut_tree b goal (inc+1)), (cut_tree c goal (inc+1)), (cut_tree d goal (inc+1)))
  | _ -> failwith "erro"


(* Função que permite contar os nodos da árvore *)
let rec count_nodes = function
  | L(a, b) -> 1
  | N(a, b, c, d) -> (count_nodes a) + (count_nodes b) + (count_nodes c) + (count_nodes d)

(* Função que devolve o menor de 4 números *)
let minor4 a b c d = min (min a b) (min c d)

(* Função que devolve a camada da folha mais alta *)
let rec highest_leaf = function
  | L(a, b) -> 0
  | N(a, b, c, d) -> minor4 (1 + highest_leaf a)  (1 + highest_leaf b)  (1 + highest_leaf c)  (1 + highest_leaf d)

(* Função que transforma uma árvore em array array de inteiros (matriz) *)
let tree_to_matrix tr arr i j level =
  let rec tree_aux tr arr bit i j level inc =
    match tr with
    | L(a, b) when (inc < level) -> tree_aux tr arr bit (i*2) (j*2) level (inc+1);
                                    tree_aux tr arr bit (i*2) (j*2 + 1) level (inc+1);
                                    tree_aux tr arr bit (i*2 + 1) (j*2) level (inc+1);
                                    tree_aux tr arr bit (i*2 + 1) (j*2 + 1) level (inc+1)
    | N(a, b, c, d) when (inc < level) -> tree_aux a arr bit (i*2) (j*2) level (inc+1);
                                          tree_aux b arr bit (i*2) (j*2 + 1) level (inc+1);
                                          tree_aux c arr bit (i*2 + 1) (j*2) level (inc+1);
                                          tree_aux d arr bit (i*2 + 1) (j*2 + 1) level (inc+1)
    | L(a, b) when (inc = level) -> arr.(i).(j) <- (leaf_bit a) 
    | _ -> failwith "erro" in
    tree_aux tr arr 20 i j level 0

(* Contém a lista tal como foi retirada do ficheiro (lista de strings) *)
let raw = build_list []

(* Devolve o último elemento de uma lista *)
let ts x = List.nth x ((List.length x) - 1)
(* Função que converte a lista 'raw' para uma lista lista de inteiros *)
let input = convert_to_int(filter_list(raw))

(* Funções auxiliares logaritmo *)
let log x = Stdlib.log x
let log2 x = log x /. log 2.
(* Lista de arrays auxiliar que será preenchida pela função tree_to_matrix *)
let dummy = Array.make_matrix (int_of_string (ts raw)) (int_of_string (ts raw)) (-1)

(* Função para remover os espaços do output final *)
let remove_space arr i j s =
  match j with
  | _ when (j < s) -> Printf.printf "%d " arr.(i).(j)
  | _ -> Printf.printf "%d\n" arr.(i).(j)

(* Função para mostrar o array final (linhas e colunas) *)
let print_matrix arr =
  let aux arr size =
    for i=0 to size-1 do
      for j=0 to size-1 do
        remove_space arr i j (size-1)
      done
    done in aux arr (Array.length arr)

(* Função principal que executa todas as funções acima listadas *)
let () = 
  let mytree = tree input and a = (int_of_string (ts raw)) in 
  let cuttree = cut_tree mytree (int_of_float (log2 (float_of_int a))) 0 in
  Printf.printf "%d\n" (highest_leaf mytree); 
  Printf.printf "%d\n" (count_nodes mytree);
  tree_to_matrix cuttree dummy 0 0 (int_of_float (log2 (float_of_int a)));
  print_matrix dummy



(* Caso prático 
 * Vamos utilizar o seguinte input:
  P1
  8 8
  0 0 0 0 1 0 1 0
  0 0 0 0 0 0 0 1
  0 0 0 0 0 0 1 1
  0 0 0 0 0 1 1 1
  0 0 0 0 1 1 1 1
  0 0 0 0 1 1 1 1
  0 0 0 1 1 1 1 1
  0 0 1 1 1 1 1 1
  4
 *
 * Começamos por passar o conteúdo do ficheiro para uma lista (raw), ficando:
 *
 * ["P1"; "8 8"; "0 0 0 0 1 0 1 0"; "0 0 0 0 0 0 0 1"; "0 0 0 0 0 0 1 1";
   "0 0 0 0 0 1 1 1"; "0 0 0 0 1 1 1 1"; "0 0 0 0 1 1 1 1"; "0 0 0 1 1 1 1 1";
   "0 0 1 1 1 1 1 1"; "4"]
 * (cada elemento é uma linha do ficheiro)
 * 
 * Aplicadas as funções build_list, filter_list e convert_to_int à lista raw, ficamos
 * com o seguinte input:
 * [[0; 0; 0; 0; 1; 0; 1; 0]; [0; 0; 0; 0; 0; 0; 0; 1];
   [0; 0; 0; 0; 0; 0; 1; 1]; [0; 0; 0; 0; 0; 1; 1; 1];
   [0; 0; 0; 0; 1; 1; 1; 1]; [0; 0; 0; 0; 1; 1; 1; 1];
   [0; 0; 0; 1; 1; 1; 1; 1]; [0; 0; 1; 1; 1; 1; 1; 1]]
 * 
 * Com o nosso input já filtrado, é aplicada a função tree que o transformará numa
 * árvore. O resultado é o seguinte:
 * 
 *  N (L (W, 16),
    N (N (L (B, -1), L (W, 1), L (W, 1), L (W, 1)),
    N (L (B, -1), L (W, 1), L (W, 1), L (B, -1)),
    N (L (W, 1), L (W, 1), L (W, 1), L (B, -1)), L (B, -4)),
    N (L (W, 4), L (W, 4), L (W, 4),
    N (L (W, 1), L (B, -1), L (B, -1), L (B, -1))),
    L (B, -16))
 * 
 * A esta árvore será aplicado um corte, na camada especificada no ficheiro de input.
 * A esse número de camada será aplicado um logaritmo de base 2 que servirá de argumento para
 * a função cut_tree. A função cut_tree cortará a árvore da seguinte maneira:
 *  N (L (W, 16), N (L (W, 2), L (B, 0), L (W, 2), L (B, -4)),
    N (L (W, 4), L (W, 4), L (W, 4), L (B, -2)), L (B, -16))
 * 
 * Resta-nos agora converter a nossa árvore já cortada para uma matriz. A função tree_to_matrix
 * necessita de uma matriz inicialmente vazia para que possa ser preenchida à medida que
 * é percorrida a árvore, resultando assim na seguinte matriz:
 * 
 * [|[|0; 0; 0; 1|]; [|0; 0; 0; 1|]; [|0; 0; 1; 1|]; [|0; 1; 1; 1|]|]
 * 
 * O output final conterá o nível da folha mais alta (através da função highest_leaf),
 * o número de folhas da árvore resultante (count_nodes) e a matriz final reduzida (conhecida vulgarmente por thumbnail).
 * 
 * O output final é, portanto:
 *  1
    22
    0 0 0 1
    0 0 0 1
    0 0 1 1
    0 1 1 1
 * 
 *)
  
