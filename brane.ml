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
  | Tag of string (* for debugging purposes *)
  [@@deriving sexp]


let hole_fold_right ~f ~init xs =
  let _, _, acc = List.fold_right 
    ~f:(fun x (hd, tl, acc) ->
      let hd' = List.drop_last_exn hd in
      hd', x :: tl, f hd' x tl acc)
    ~init:(xs, [], init)
    xs
  in acc

let%expect_test "hole fold right test" =
  hole_fold_right
    ~f:(fun hd x tl ys -> (hd, x, tl) :: ys)
    ~init:[]
    [0; 1; 2] 
  |> List.sexp_of_t (Tuple3.sexp_of_t 
    (List.sexp_of_t sexp_of_int)
    sexp_of_int
    (List.sexp_of_t sexp_of_int))
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    ((() 0 (1 2)) ((0) 1 (2)) ((0 1) 2 ())) |}]

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

let exo_step b = 
  let { name; interior } = b in 
  hole_concat_map name
    ~f:(fun op_hd1 op op_tl1 -> 
      match op with
      | CoExo outer1 ->
        hole_concat_map interior
          ~f:(fun br_hd br br_tl ->
            hole_concat_map br.name
              ~f:(fun op_hd2 op op_tl2 ->
                match op with
                | Exo outer2 -> 
                  [{ name = op_hd2 @ outer2 @ op_tl2 
                    @ op_hd1 @ outer1 @ op_tl1
                    ; interior = br_hd @ br_tl } 
                  :: br.interior]
                | _ -> []))
      | _ -> [])

let pino_step b =
  let { name; interior } = b in
  hole_concat_map name
    ~f:(fun hd op tl ->
      match op with 
      | Pino { inner; outer } -> 
        [[{ name = hd @ outer @ tl
        ; interior = 
          { name = inner
          ; interior = [] }
          :: interior }]]
      | _ -> [])

let rec step_system bs =
  (* phago step *)
  phago_step bs
  (* recursive step *)
  @ hole_concat_map
    ~f:(fun hd b tl ->
      List.map
        ~f:(fun bs' -> hd @ bs' @ tl)
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
      [{ name = name
      ; interior = interior' }])
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

let%expect_test "basic phago eval" =
  print_system_eval_tree
    [{ name = [Tag "sigma1"; Phago [Tag "sigma"]; Tag "sigma2"]; 
      interior = 
        [{ name = [Tag "p1"]; interior = [] };
        { name = [Tag "p2"]; interior = [] }] };
    { name = [Tag "tau1"; CoPhago { inner = [Tag "rho"]; outer = [Tag "tau"] }; Tag "tau2"]; 
      interior = 
        [{ name = [Tag "q1"]; interior = [] };
        { name = [Tag "q2"]; interior = [] }] } ];
  [%expect {|
    (Node
     (((name ((Tag sigma1) (Phago ((Tag sigma))) (Tag sigma2)))
       (interior
        (((name ((Tag p1))) (interior ())) ((name ((Tag p2))) (interior ())))))
      ((name
        ((Tag tau1) (CoPhago (inner ((Tag rho))) (outer ((Tag tau)))) (Tag tau2)))
       (interior
        (((name ((Tag q1))) (interior ())) ((name ((Tag q2))) (interior ()))))))
     ((Node
       (((name ((Tag tau1) (Tag tau) (Tag tau2)))
         (interior
          (((name ((Tag rho)))
            (interior
             (((name ((Tag sigma1) (Tag sigma) (Tag sigma2)))
               (interior
                (((name ((Tag p1))) (interior ()))
                 ((name ((Tag p2))) (interior ()))))))))
           ((name ((Tag q1))) (interior ())) ((name ((Tag q2))) (interior ()))))))
       ()))) |}]

let%expect_test "basic exo eval" =
  print_system_eval_tree [
    { name = [Tag "tau1"; CoExo [Tag "tau"]; Tag "tau2"]
    ; interior = [
      { name = [Tag "q1"]; interior = [] };
      { name = [Tag "sigma1"; Exo [Tag "sigma"]; Tag "sigma2"]
      ; interior = [
        { name = [Tag "p1"]; interior = [] };
        { name = [Tag "p2"]; interior = [] }
      ]};
      { name = [Tag "q2"]; interior = [] }
    ]}];
  [%expect {|
    (Node
     (((name ((Tag tau1) (CoExo ((Tag tau))) (Tag tau2)))
       (interior
        (((name ((Tag q1))) (interior ()))
         ((name ((Tag sigma1) (Exo ((Tag sigma))) (Tag sigma2)))
          (interior
           (((name ((Tag p1))) (interior ())) ((name ((Tag p2))) (interior ())))))
         ((name ((Tag q2))) (interior ()))))))
     ((Node
       (((name
          ((Tag sigma1) (Tag sigma) (Tag sigma2) (Tag tau1) (Tag tau) (Tag tau2)))
         (interior
          (((name ((Tag q1))) (interior ())) ((name ((Tag q2))) (interior ())))))
        ((name ((Tag p1))) (interior ())) ((name ((Tag p2))) (interior ())))
       ()))) |}]

let%expect_test "basic pino eval" =
  print_system_eval_tree
    [{ name = [Pino { inner = [Tag "rho"]; outer = [Tag "tau"] }; Tag "sigma"]
    ; interior = 
      [{ name = [Tag "p1"]; interior = [] };
      { name = [Tag "p2"]; interior = [] }] }];
  [%expect {|
    (Node
     (((name ((Pino (inner ((Tag rho))) (outer ((Tag tau)))) (Tag sigma)))
       (interior
        (((name ((Tag p1))) (interior ())) ((name ((Tag p2))) (interior ()))))))
     ((Node
       (((name ((Tag tau) (Tag sigma)))
         (interior
          (((name ((Tag rho))) (interior ())) ((name ((Tag p1))) (interior ()))
           ((name ((Tag p2))) (interior ()))))))
       ()))) |}]
