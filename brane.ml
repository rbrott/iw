open Core

(* nests of membranes *)
type system = brane list
  [@@deriving sexp]
and brane =
  { name: action
  ; interior: system }
  [@@deriving sexp]
(* bitonal actions *)
and action = op list
  [@@deriving sexp]
and op =
  | Phago
  | CoPhago of { inner: action; outer: action }
  | Exo
  | CoExo of action
  | Pino of { inner: action; outer: action }
  [@@deriving sexp]


let hole_fold_right ~f ~init xs =
  let _, _, acc = List.fold_right 
    ~f:(fun x (hd, tl, acc) ->
      let tl' = List.tl_exn tl in
      hd @ [x], tl', f hd x tl' acc)
    ~init:([], xs, init)
    xs
  in acc

let hole_map_concat ~f =
  hole_fold_right
    ~f:(fun hd x tl acc ->
      f hd x tl @ acc)
    ~init:[]

let rec step_system bs =
  hole_map_concat
    ~f:(fun hd b tl ->
      List.map
        ~f:(fun b' -> hd @ [b'] @ tl)
        (step_brane b))
    bs
and step_brane { name; interior } =
  (* pino step *)
  hole_map_concat
    ~f:(fun hd op tl ->
      match op with 
      | Pino { inner; outer } -> 
        [{ name = hd @ outer @ tl
        ; interior = 
          [{ name = inner
          ; interior = [] }] }]
      | _ -> [])
    name
  (* recursive step *)
  @ List.map
    ~f:(fun interior' ->
      { name = name
      ; interior = interior' })
    (step_system interior)

type 'a tree = 
  | Node of 'a * 'a tree list
  [@@deriving sexp]

let rec eval_tree e n x = Node (x, 
  if n <= 0 then []
  else List.map ~f:(eval_tree e (n - 1)) (e x))

let print_system_eval_tree s = 
  s |> eval_tree step_system 10
  |> sexp_of_tree sexp_of_system
  |> Sexp.to_string_hum |> print_endline

let%expect_test "pino eval" =
  print_system_eval_tree
    [{ name = [Pino { inner = []; outer = [] }]
    ; interior = [] }];
  [%expect {|
    (Node (((name ((Pino (inner ()) (outer ())))) (interior ())))
     ((Node (((name ()) (interior (((name ()) (interior ())))))) ()))) |}]
