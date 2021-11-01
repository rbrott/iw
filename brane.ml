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


let mate bs = Phago [{ arep = false; op = Exo bs }]
let comate bs = CoPhago { 
  inner = [{ arep = false; op = CoExo [{ arep = false; op = Exo [] }] }]; 
  outer = [{ arep = false; op = CoExo bs }] }

let bud bs = Phago bs
let cobud inner outer = Pino {
  inner = [{ arep = false; op = CoPhago { 
    inner = inner; outer = [{ arep = false; op = Exo[] }] }}];
  outer = [{ arep = false; op = CoExo outer}] }

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

let rec map_tree ~f = function
  | Node (x, xs) -> Node (f x, List.map ~f:(map_tree ~f) xs)

let print_system_eval_tree s = 
  s |> eval_tree step_system 10
  |> sexp_of_tree sexp_of_sys
  |> Sexp.to_string_hum |> print_endline


type op_token =
  | PhagoTok
  | CoPhagoTok
  | ExoTok
  | CoExoTok
  | PinoTok
  | MateTok
  | CoMateTok
  | BudTok
  | CoBudTok
  [@@deriving sexp, equal]

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
  | Let
  | In
  | Eof
  | Op of op_token
  [@@deriving sexp, equal]

type lex_error =
  | BadCharacter of char
  [@@deriving sexp]

let promote_reserved s = 
  if String.equal s "let" then Let
  else if String.equal s "in" then In
  else if String.equal s "phago" then Op PhagoTok 
  else if String.equal s "cophago" then Op CoPhagoTok 
  else if String.equal s "exo" then Op ExoTok
  else if String.equal s "coexo" then Op CoExoTok
  else if String.equal s "pino" then Op PinoTok
  else if String.equal s "mate" then Op MateTok
  else if String.equal s "comate" then Op CoMateTok
  else if String.equal s "bud" then Op BudTok
  else if String.equal s "cobud" then Op CoBudTok 
  else Id s
  
let tokens_of_string s = 
  let tl = ref (String.to_list s) in
  let peek () = List.hd !tl in
  let next () = tl := List.tl_exn !tl in
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
      | `Id id, _ -> Ok `Begin, Some (promote_reserved id)
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
     (LParen Bang (Op ExoTok) LParen LBracket RBracket RParen Dot LBracket
      RBracket RParen LBracket RBracket)) |}] 

type parse_error =
  | BadToken of { exp: token; actual: token }
  | ExpectedId of token
  | ExpectedOp of token
  | NoActionForId of string
  | NoSysForId of string
  | PrematureEof
  [@@deriving sexp]

let sys_of_tokens tokens =
  let open Result.Let_syntax in
  let tl = ref tokens in
  let peek () = Option.value (List.hd !tl) ~default:Eof in
  let peek2 () = 
    match !tl with
    | _ :: x :: _ -> x
    | _ -> Eof 
  in
  let next () = tl := Option.value (List.tl !tl) ~default:[] in
  let eat exp_token =
    let actual_token = peek () in
    if equal_token exp_token actual_token then 
      (next (); Ok ())
    else 
      Error (BadToken { exp = exp_token; actual = actual_token })
  in 
  let eat_opt exp_token =
    if equal_token exp_token (peek ()) 
      then (next (); true) else false 
  in
  let eat_many ~sep ?close eat_item =
    let rec eat_one () =
      let%bind item = eat_item () in
      if equal_token (peek ()) sep then 
        (next ();
        let%bind tl = eat_one () in
        Ok (item :: tl))
      else
        Ok [item]
    in
    match close with
    | None -> eat_one ()
    | Some close ->
      if equal_token (peek ()) close then
        Ok []
      else
        eat_one ()
  in
  let eat_id () =
    match peek () with
    | Id s -> next (); Ok s
    | token -> Error (ExpectedId token)
  in
  let rec eat_action act_bs = 
    let arep = eat_opt Bang in 
    match peek () with
    | Op op_tok -> next(); let%bind op =
      (match op_tok with
      | PhagoTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (Phago actions)
      | CoPhagoTok -> 
        let%bind () = eat LParen in
        let%bind inner = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind outer = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (CoPhago { inner; outer })
      | ExoTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (Exo actions)
      | CoExoTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (CoExo actions)
      | PinoTok -> 
        let%bind () = eat LParen in
        let%bind inner = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind outer = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (Pino{ inner; outer })
      | MateTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (mate actions)
      | CoMateTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (comate actions)
      | BudTok -> 
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind actions = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (bud actions)
      | CoBudTok -> 
        let%bind () = eat LParen in
        let%bind inner = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind outer = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (cobud inner outer)) 
      in
      Ok [{ arep; op }]
    | Id action_id -> next(); begin
      match Map.find act_bs action_id with
      | Some actions -> Ok actions
      | None -> Error (NoActionForId action_id)
      end
    | token -> Error (ExpectedOp token)
  and eat_actions ?close act_bs = 
    let%bind actions = eat_many
      (fun () -> eat_action act_bs) ~sep:Comma ?close in
    Ok (List.concat actions)
  in
  let rec eat_brane sys_bs act_bs = 
    match peek () with 
    | Id sys_id -> next(); begin
      match Map.find sys_bs sys_id with
      | Some sys -> Ok sys 
      | None -> Error (NoSysForId sys_id)
      end
    | _ -> 
      let brep = eat_opt Bang in
      let%bind () = eat LParen in  
      let%bind actions = eat_actions ~close:RParen act_bs in
      let%bind () = eat RParen in  
      let%bind () = eat LBracket in 
      let%bind contents = eat_sys ~close:RParen sys_bs act_bs in
      let%bind () = eat RBracket in 
      Ok [{ brep; actions; contents }]
  and eat_sys ?close sys_bs act_bs = 
    let%bind sys = eat_many ~sep:Comma ?close (fun () -> eat_brane sys_bs act_bs) in 
    Ok (List.concat sys)
  in 
  let eat_sys_or_actions sys_bs act_bs =
    match peek () with
    | Eof -> Error PrematureEof
    | Bang -> begin 
      match peek2 () with
      | LParen | LBracket -> 
        let%bind sys = eat_sys sys_bs act_bs in
        Ok (`System sys)
      | _ -> 
        let%bind actions = eat_actions act_bs in
        Ok (`Actions actions)
      end
    | LParen | LBracket -> 
      let%bind sys = eat_sys sys_bs act_bs in
      Ok (`System sys)
    | _ -> 
      let%bind actions = eat_actions act_bs in
      Ok (`Actions actions)
  in
  let rec eat_let sys_bs act_bs =
    let%bind () = eat Let in
    let%bind key = eat_id () in
    let%bind () = eat Equal in
    let%bind value = eat_sys_or_actions sys_bs act_bs in
    let%bind () = eat In in
    match value with
    | `System sys -> eat_exp (Map.set sys_bs ~key ~data:sys) act_bs
    | `Actions actions -> eat_exp sys_bs (Map.set act_bs ~key ~data:actions)
  and eat_exp sys_bs act_bs = 
    match peek () with
    | Let -> eat_let sys_bs act_bs
    | _ -> eat_sys sys_bs act_bs
  in
  let%bind exp = eat_exp (Map.empty (module String)) (Map.empty (module String)) in
  let%bind () = eat Eof in 
  Ok exp

let indent ss = ss
  |> String.split_lines
  |> List.map ~f:(fun s -> " " ^ s)
  |> String.concat ~sep:"\n"

let string_of_list ~sep string_of_x xs = xs
  |> List.map ~f:string_of_x
  |> String.concat ~sep

let rec string_of_sys bs =
  string_of_list ~sep:",\n" string_of_brane bs
and string_of_brane b =
  let { actions; brep; contents } = b in 
  Printf.sprintf "%s(%s)[\n%s]" 
    (if brep then "!" else "")
    (actions |> string_of_actions)
    (contents|> string_of_sys |> indent)
and string_of_action a = 
  let { arep; op } = a in
  (if arep then "!" else "") ^
  (string_of_op op)
and string_of_actions a = string_of_list ~sep:", " string_of_action a
and string_of_op = function
  | Phago actions ->
    Printf.sprintf "phago. (%s)" (string_of_actions actions)
  | CoPhago { inner; outer } ->
    Printf.sprintf "cophago(%s). (%s)" 
      (string_of_actions inner)
      (string_of_actions outer)
  | Exo actions ->
    Printf.sprintf "exo. (%s)" (string_of_actions actions)
  | CoExo actions ->
    Printf.sprintf "coexo. (%s)" (string_of_actions actions)
  | Pino { inner; outer } ->
    Printf.sprintf "pino(%s). (%s)" 
      (string_of_actions inner)
      (string_of_actions outer)
  | Tag _ -> failwith "tag"


type error =
  | LexError of lex_error
  | ParseError of parse_error
  [@@deriving sexp]

let sys_of_string s = 
  match tokens_of_string s with 
  | Error err -> Error (LexError err)
  | Ok tokens ->
    (match sys_of_tokens tokens with
    | Error err -> Error (ParseError err)
    | Ok sys -> Ok sys)

let%expect_test "basic phago eval" = "let a = mate.() in let b = !coexo.() in (a, b)[]"
  |> sys_of_string
  |> Result.sexp_of_t sexp_of_sys sexp_of_error
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    (Ok
     (((actions
        (((op (Phago (((op (Exo ())) (arep false))))) (arep false))
         ((op (CoExo ())) (arep true))))
       (brep false) (contents ())))) |}]

let%expect_test "basic phago eval" = 
  let s = "let a = mate.() in let b = !coexo.() in (a, b)[]" in
  (match sys_of_string s with
  | Ok sys -> print_endline (string_of_sys sys)
  | Error err -> print_endline (Sexp.to_string_hum (sexp_of_error err)));
  [%expect{|
    (phago. (exo. ()), !coexo. ())[
    ] |}];
  [%expect {| |}]

(* 
(* TODO: work on this further *)
let%expect_test "basic phago eval" = "
let nucap = (!bud)[] in
let virus = phago.(exo.())[nucap] in
"
  |> sys_of_string
  |> Result.sexp_of_t sexp_of_sys sexp_of_error
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    (Ok
     (((actions (((op (Exo ())) (arep false)) ((op (CoExo ())) (arep true))))
       (brep false) (contents ())))) |}] *)

(* let%expect_test "basic phago eval" = "let a = exo.() in (a)[]"
  |> tokens_of_string
  |> Result.sexp_of_t (List.sexp_of_t sexp_of_token) sexp_of_lex_error
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    (Ok
     (Let (Id a) Equal (Op ExoTok) Dot LParen RParen In LParen (Id a) RParen
      LBracket RBracket)) |}] *)
  

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
