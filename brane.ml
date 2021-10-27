open Core

(* nests of membranes *)
type system = brane list
  [@@deriving sexp]
and brane =
  { name: action
  (* maybe contents? *)
  ; interior: system }
  [@@deriving sexp]
(* bitonal actions *)
and action = op list
  [@@deriving sexp]
and op =
  | Phago of action
  | CoPhago of { inner: action; outer: action }
  | Exo of action
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

let hole_concat_map ~f =
  hole_fold_right
    ~f:(fun hd x tl acc ->
      f hd x tl @ acc)
    ~init:[]

let phago_step bs =
  hole_concat_map bs
    ~f:(fun br_hd br br_tl ->
      let { name; interior = inner_int } = br in
      hole_concat_map name
        ~f:(fun op_hd op op_tl ->
          match op with
          | Phago inner -> [
            { name = op_hd @ inner @ op_tl
            ; interior = inner_int }, 
            br_hd @ br_tl]
          | _ -> []))
  |> List.concat_map
    ~f:(fun (br_inner, bs') ->
      hole_concat_map bs'
        ~f:(fun br_hd br_outer br_tl ->
          let { name; interior = middle_int } = br_outer in
          hole_concat_map name
            ~f:(fun op_hd op op_tl ->
              match op with
              | CoPhago { inner = middle; outer } -> 
                [br_hd @ 
                  [{ name = op_hd @ outer @ op_tl
                  ; interior = 
                    { name = middle
                    ; interior = [br_inner] }
                    :: middle_int }]
                  @ br_tl]
              | _ -> [])))

let exo_step _ = []

let pino_step b =
  let { name; interior } = b in
  hole_concat_map name
    ~f:(fun hd op tl ->
      match op with 
      | Pino { inner; outer } -> 
        [{ name = hd @ outer @ tl
        ; interior = 
          { name = inner
          ; interior = [] }
          :: interior }]
      | _ -> [])

let rec step_system bs =
  (* phago step *)
  phago_step bs
  (* recursive step *)
  @ hole_concat_map
    ~f:(fun hd b tl ->
      List.map
        ~f:(fun b' -> hd @ [b'] @ tl)
        (step_brane b))
    bs

and step_brane b =
  (* pino step *)
  pino_step b
  (* exo step *)
  @ exo_step b
  (* recursive step *)
  @ let { name; interior } = b in
    List.map
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

let%expect_test "basic pino eval" =
  print_system_eval_tree
    [{ name = [Pino { inner = []; outer = [] }]
    ; interior = [] }];
  [%expect {|
    (Node (((name ((Pino (inner ()) (outer ())))) (interior ())))
     ((Node (((name ()) (interior (((name ()) (interior ())))))) ()))) |}]

let%expect_test "basic phago eval" =
  print_system_eval_tree
    [{ name = [Phago []]; interior = [] }
    ;{ name = [CoPhago { inner = []; outer = [] }]; interior = [] }];
  [%expect {|
    (Node
     (((name ((Phago ()))) (interior ()))
      ((name ((CoPhago (inner ()) (outer ())))) (interior ())))
     ((Node
       (((name ())
         (interior (((name ()) (interior (((name ()) (interior ())))))))))
       ()))) |}]
