open Core

(* nests of membranes *)
type system =
  | Empty
  | Compose of system * system
  | Replicate of system
  | Membrane of brane * system
  [@@deriving sexp]
(* combinations of actions *)
and brane = string
  (* | Void
  | Join of brane * brane
  | Fix of brane
  | Action of action * brane
(* bitonal actions *)
and action =
  | Phago
  | CoPhago of brane
  | Exo
  | CoExo
  | Pino of brane *)

(* for now, don't guard fixed points *)
(* embrace nondeterminism in the small-step relation? *)
let rec step_system = function
| Empty -> []
| Compose (Empty, p) -> p :: 
  List.map ~f:(fun p' -> Compose (Empty, p')) (step_system p)
| Compose (p, Empty) -> p ::
  List.map ~f:(fun p' -> Compose (p', Empty)) (step_system p)
| Compose (p, q) -> 
  List.map ~f:(fun p' -> Compose (p', q)) (step_system p) @
  List.map ~f:(fun q' -> Compose (p, q')) (step_system q)
(* TODO: this isn't quite right... Empty is special and should expand if necessary *)
| Replicate Empty -> [Empty]
(* TODO: Replicate Replicate ... *)
| Replicate p ->
  Compose (p, Replicate p) ::
  List.map ~f:(fun p' -> Replicate p') (step_system p)
(* | Membrane (Void, Empty) -> [Empty] *)
| Membrane (b, p) -> 
  List.map ~f:(fun b' -> Membrane (b', p)) (step_brane b) @
  List.map ~f:(fun p' -> Membrane (b, p')) (step_system p)
and step_brane = function
| _ -> failwith ""

let rec dot_of_system i = function
| Empty -> i + 1, Printf.sprintf "empty_%d[shape=none,label=\"∅\"]" i
| Compose (p, q) ->
  let i, pdot = dot_of_system i p in
  let i, qdot = dot_of_system i q in
  i + 1, pdot ^ " " ^ qdot
| Replicate p ->
  let i, pdot = dot_of_system i p in
  i + 1, Printf.sprintf "subgraph cluster_%d { style=dashed; %s }" i pdot
| Membrane (b, p) -> 
  let i, pdot = dot_of_system i p in
  i + 1, Printf.sprintf "subgraph cluster_%d { style=solid; label=\"%s\"; %s }" i b pdot

type 'a tree = 
  | Node of 'a * 'a tree list
  [@@deriving sexp]

let rec eval_tree e n x = Node (x, 
  if n <= 0 then []
  else List.map ~f:(eval_tree e (n - 1)) (e x))

(* let rec eval_dag e n x = Node (x, ) *)

let print_system_eval_tree s = 
  s |> eval_tree step_system 10
  |> sexp_of_tree sexp_of_system
  |> Sexp.to_string_hum |> print_endline

let s1 = Compose (Empty, Compose (Empty, Empty))

let%expect_test "empty eval" =
  print_system_eval_tree s1;
  [%expect {|
    (Node (Compose Empty (Compose Empty Empty))
     ((Node (Compose Empty Empty) ((Node Empty ())))
      (Node (Compose Empty Empty) ((Node Empty ()))))) |}]
