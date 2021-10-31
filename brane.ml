open Core

type action = 
  { op: op
  ; arep: bool }
  [@@deriving sexp]
and op =
  | Phago of action list
  | CoPhago of { inner: action list; outer: action list }
  | Exo of action list
  | CoExo of action list
  | Pino of { inner: action list; outer: action list }
  | Tag of string (* for debugging purposes *)
  [@@deriving sexp]

type sys = brane list
  [@@deriving sexp]
and brane = 
  { actions: action list
  ; brep: bool
  ; contents: sys }
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

(* brane and action residue *)
let br_res b = if b.brep then [b] else []
let ac_res a = if a.arep then [a] else []

let phago_step bs =
  (* produces inner branes with previous siblings *)
  hole_concat_map bs
    ~f:(fun br_hd br br_tl ->
      let { actions; contents = cont_inner; _ } = br in
      hole_concat_map actions
        ~f:(fun ac_hd ac ac_tl ->
          match ac.op with
          | Phago ac_inner -> [
            { actions = ac_hd @ ac_inner @ ac_res ac @ ac_tl
            ; brep = false
            ; contents = cont_inner }, 
            br_hd @ br_res br @ br_tl]
          | _ -> []))
  (* match them with outer branes *)
  |> List.concat_map
    ~f:(fun (br_inner, bs') ->
      hole_concat_map bs'
        ~f:(fun br_hd br_outer br_tl ->
          let { actions; contents = cont_middle; _ } = br_outer in
          hole_concat_map actions
            ~f:(fun ac_hd ac ac_tl ->
              match ac.op with
              | CoPhago { inner = ac_middle; outer = ac_outer } -> 
                [br_hd @ 
                  [{ actions = ac_hd @ ac_outer @ ac_res ac @ ac_tl
                  ; brep = false
                  ; contents = 
                    { actions = ac_middle
                    ; brep = false
                    ; contents = [br_inner] }
                    :: cont_middle }]
                  @ br_res br_outer @ br_tl]
              | _ -> [])))

let exo_step br_outer =  
  let { actions; contents = cont_outer; _ } = br_outer in 
  hole_concat_map actions
    ~f:(fun ac_hd1 ac1 ac_tl1 -> 
      match ac1.op with
      | CoExo ac_outer1 ->
        hole_concat_map cont_outer
          ~f:(fun br_hd br_inner br_tl ->
            let { actions; contents = cont_inner; _ } = br_inner in
            hole_concat_map actions
              ~f:(fun ac_hd2 ac2 ac_tl2 ->
                match ac2.op with
                | Exo ac_outer2 -> [
                  { actions = ac_hd2 @ ac_outer2 @ ac_res ac2 @ ac_tl2 
                    @ ac_hd1 @ ac_outer1 @ ac_res ac2 @ ac_tl1
                  ; brep = false
                  ; contents = br_hd @ br_res br_inner @ br_tl } 
                  :: cont_inner ]
                | _ -> []))
      | _ -> [])

let pino_step br =
  let { actions; contents; _ } = br in
  hole_concat_map actions
    ~f:(fun ac_hd ac ac_tl ->
      match ac.op with 
      | Pino { inner = ac_inner; outer = ac_outer } -> 
        [[{ actions = ac_hd @ ac_outer @ ac_tl
        ; brep = false
        ; contents = 
          { actions = ac_inner
          ; brep = false
          ; contents = [] }
          :: contents }]]
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
  @ let { actions; brep; contents } = b in
    List.map
    ~f:(fun contents' ->
      [{ actions = actions
      ; brep = brep
      ; contents = contents' }])
    (step_system contents)

type 'a tree = 
  | Node of 'a * 'a tree list
  [@@deriving sexp]

let rec eval_tree e n x = Node (x, 
  if n <= 0 then []
  else List.map ~f:(eval_tree e (n - 1)) (e x))

let print_system_eval_tree s = 
  s |> eval_tree step_system 10
  |> sexp_of_tree sexp_of_sys
  |> Sexp.to_string_hum |> print_endline


type token =
  | Id of string
  | Dot
  | Bang
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Comma
  | Equal
  | Eof
  [@@deriving sexp]

type lex_error =
  | BadCharacter of char
  [@@deriving sexp]
  
let tokens_of_string s = 
  let tl = ref (String.to_list s) in
  let peek () = List.hd !tl in
  let next () = 
    tl := 
      match !tl with
      | [] -> []
      | _ :: tl' -> tl'
  in
  let append s c = s ^ (String.make 1 c) in
  let rec step state tokens_rev =
    let state', token_opt =
      match state, peek () with
      (* whitespace *)
      | `Begin, Some ' ' 
      | `Begin, Some '\n' 
      | `Begin, Some '\r' 
      | `Begin, Some '\t' -> next (); Ok `Begin, None
      (* identifiers *)
      | `Begin, Some ('A'..'Z' as c)
      | `Begin, Some ('a'..'z' as c) -> next (); Ok (`Id (append "" c)), None
      | `Id id, Some ('A'..'Z' as c)
      | `Id id, Some ('a'..'z' as c)
      | `Id id, Some ('0'..'9' as c) 
      | `Id id, Some ('_' as c) -> next (); Ok (`Id (append id c)), None
      | `Id id, _ -> Ok `Begin, Some (Id id)
      (* punctuation *)
      | `Begin, Some ',' -> next (); Ok `Begin, Some Comma
      | `Begin, Some '.' -> next (); Ok `Begin, Some Dot
      | `Begin, Some '!' -> next (); Ok `Begin, Some Bang
      | `Begin, Some '=' -> next (); Ok `Begin, Some Equal
      | `Begin, Some '(' -> next (); Ok `Begin, Some LParen
      | `Begin, Some ')' -> next (); Ok `Begin, Some RParen
      | `Begin, Some '[' -> next (); Ok `Begin, Some LBracket
      | `Begin, Some ']' -> next (); Ok `Begin, Some RBracket
      (* errors *)
      | `Begin, Some c -> Error (BadCharacter c), None
      (* end *)
      | `Begin, None -> Ok `Begin, Some Eof
    in
    match state', token_opt with
    (* TODO: return preceding tokens with error? *)
    | Error err, _ -> Error err
    | Ok state', None -> step state' tokens_rev
    | _, Some Eof -> Ok (List.rev tokens_rev)
    | Ok state', Some token -> step state' (token :: tokens_rev)
  in
  step `Begin []

let print_tokens_of_string s = 
  s
    |> tokens_of_string 
    |> Result.sexp_of_t (List.sexp_of_t sexp_of_token) sexp_of_lex_error
    |> Sexp.to_string_hum
    |> print_endline

let%expect_test "lex brane" =
  print_tokens_of_string "(!exo([]).[])[]"; 
  [%expect {|
    (Ok
     (LParen Bang (Id exo) LParen LBracket RBracket RParen Dot LBracket RBracket
      RParen LBracket RBracket)) |}] 

type ast =
  | Sys of sys
  | LetSys of string * sys
  | LetAction of string * action
  [@@deriving sexp]

(* 
let%expect_test "basic phago eval" =
  print_system_eval_tree
    [{ actions = [Tag "sigma1"; Phago [Tag "sigma"]; Tag "sigma2"]; 
      contents = 
        [{ actions = [Tag "p1"]; contents = [] };
        { actions = [Tag "p2"]; contents = [] }] };
    { actions = [Tag "tau1"; CoPhago { inner = [Tag "rho"]; outer = [Tag "tau"] }; Tag "tau2"]; 
      contents = 
        [{ actions = [Tag "q1"]; contents = [] };
        { actions = [Tag "q2"]; contents = [] }] } ];
  [%expect {|
    (Node
     (((actions ((Tag sigma1) (Phago ((Tag sigma))) (Tag sigma2)))
       (contents
        (((actions ((Tag p1))) (contents ())) ((actions ((Tag p2))) (contents ())))))
      ((actions
        ((Tag tau1) (CoPhago (inner ((Tag rho))) (outer ((Tag tau)))) (Tag tau2)))
       (contents
        (((actions ((Tag q1))) (contents ())) ((actions ((Tag q2))) (contents ()))))))
     ((Node
       (((actions ((Tag tau1) (Tag tau) (Tag tau2)))
         (contents
          (((actions ((Tag rho)))
            (contents
             (((actions ((Tag sigma1) (Tag sigma) (Tag sigma2)))
               (contents
                (((actions ((Tag p1))) (contents ()))
                 ((actions ((Tag p2))) (contents ()))))))))
           ((actions ((Tag q1))) (contents ())) ((actions ((Tag q2))) (contents ()))))))
       ()))) |}]

let%expect_test "basic exo eval" =
  print_system_eval_tree [
    { actions = [Tag "tau1"; CoExo [Tag "tau"]; Tag "tau2"]
    ; contents = [
      { actions = [Tag "q1"]; contents = [] };
      { actions = [Tag "sigma1"; Exo [Tag "sigma"]; Tag "sigma2"]
      ; contents = [
        { actions = [Tag "p1"]; contents = [] };
        { actions = [Tag "p2"]; contents = [] }
      ]};
      { actions = [Tag "q2"]; contents = [] }
    ]}];
  [%expect {|
    (Node
     (((actions ((Tag tau1) (CoExo ((Tag tau))) (Tag tau2)))
       (contents
        (((actions ((Tag q1))) (contents ()))
         ((actions ((Tag sigma1) (Exo ((Tag sigma))) (Tag sigma2)))
          (contents
           (((actions ((Tag p1))) (contents ())) ((actions ((Tag p2))) (contents ())))))
         ((actions ((Tag q2))) (contents ()))))))
     ((Node
       (((actions
          ((Tag sigma1) (Tag sigma) (Tag sigma2) (Tag tau1) (Tag tau) (Tag tau2)))
         (contents
          (((actions ((Tag q1))) (contents ())) ((actions ((Tag q2))) (contents ())))))
        ((actions ((Tag p1))) (contents ())) ((actions ((Tag p2))) (contents ())))
       ()))) |}]

let%expect_test "basic pino eval" =
  print_system_eval_tree
    [{ actions = [Pino { inner = [Tag "rho"]; outer = [Tag "tau"] }; Tag "sigma"]
    ; contents = 
      [{ actions = [Tag "p1"]; contents = [] };
      { actions = [Tag "p2"]; contents = [] }] }];
  [%expect {|
    (Node
     (((actions ((Pino (inner ((Tag rho))) (outer ((Tag tau)))) (Tag sigma)))
       (contents
        (((actions ((Tag p1))) (contents ())) ((actions ((Tag p2))) (contents ()))))))
     ((Node
       (((actions ((Tag tau) (Tag sigma)))
         (contents
          (((actions ((Tag rho))) (contents ())) ((actions ((Tag p1))) (contents ()))
           ((actions ((Tag p2))) (contents ()))))))
       ()))) |}]

let mate bs = Phago [Exo bs]
let comate bs = CoPhago { inner = [CoExo [Exo []]]; outer = [CoExo bs] }

let%expect_test "basic mate" =
  print_system_eval_tree [
    { actions = [mate [Tag "Left"]]; contents = [] };
    { actions = [comate [Tag "Right"]]; contents = [] } ];
  [%expect {|
    (Node
     (((actions ((Phago ((Exo ((Tag Left))))))) (contents ()))
      ((actions
        ((CoPhago (inner ((CoExo ((Exo ()))))) (outer ((CoExo ((Tag Right))))))))
       (contents ())))
     ((Node
       (((actions ((CoExo ((Tag Right)))))
         (contents
          (((actions ((CoExo ((Exo ())))))
            (contents (((actions ((Exo ((Tag Left))))) (contents ())))))))))
       ((Node
         (((actions ((CoExo ((Tag Right)))))
           (contents (((actions ((Tag Left) (Exo ()))) (contents ()))))))
         ((Node (((actions ((Tag Left) (Tag Right))) (contents ()))) ()))))))) |}]

let bud bs = Phago bs
let cobud inner outer = Pino 
  { inner = [CoPhago { inner = inner; outer = [Exo[]] }]
  ; outer = [CoExo outer] }

let%expect_test "basic bud" =
  print_system_eval_tree [
    { actions = [cobud [] []]
    ; contents = [
      { actions = [bud []; Tag "P"]
      ; contents = [] };
      { actions = [Tag "Q"] 
      ; contents = [] } ] } ];
  [%expect {|
    (Node
     (((actions
        ((Pino (inner ((CoPhago (inner ()) (outer ((Exo ()))))))
          (outer ((CoExo ()))))))
       (contents
        (((actions ((Phago ()) (Tag P))) (contents ()))
         ((actions ((Tag Q))) (contents ()))))))
     ((Node
       (((actions ((CoExo ())))
         (contents
          (((actions ((CoPhago (inner ()) (outer ((Exo ())))))) (contents ()))
           ((actions ((Phago ()) (Tag P))) (contents ()))
           ((actions ((Tag Q))) (contents ()))))))
       ((Node
         (((actions ((CoExo ())))
           (contents
            (((actions ((Exo ())))
              (contents
               (((actions ()) (contents (((actions ((Tag P))) (contents ()))))))))
             ((actions ((Tag Q))) (contents ()))))))
         ((Node
           (((actions ()) (contents (((actions ((Tag Q))) (contents ())))))
            ((actions ()) (contents (((actions ((Tag P))) (contents ()))))))
           ()))))))) |}]

(* TODO: do we need drip? *)

let%expect_test "basic phago eval" =
  print_system_eval_tree [
    { actions = [{ op = Phago []; arep = true }]
    ; brep = false
    ; contents = [] };
    { actions = [{ op = CoPhago { inner = []; outer = [] }; arep = true }]
    ; brep = false
    ; contents = [] }];
  [%expect {| |}]
 *)
