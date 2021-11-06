open Core

type molecule = string
  [@@deriving sexp, equal]
type action = 
  { op: op
  ; arep: bool }
  [@@deriving sexp]
and op =
  (* brane interactions *)
  | Phago of action list
  | CoPhago of { inner: action list; outer: action list }
  | Exo of action list
  | CoExo of action list
  | Pino of { inner: action list; outer: action list }
  (* molecule interactions *)
  | BindRelease of 
    { bind_out: molecule list; bind_in: molecule list
    ; release_out: molecule list; release_in: molecule list
    ; arg: action list }
  [@@deriving sexp]

type sys = sys_elt list
  [@@deriving sexp]
and sys_elt =
  | Brane of brane
  | Molecule of string
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

(* TODO: continuation, next, or something else? *)
let drip arg cont = Pino 
  { inner = [{ arep = false; op = Pino { inner = arg; outer = [{ arep = false; op = Exo [] }]}}]
  ; outer = [{ arep = false; op = CoExo cont }]
  }

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
let br_res b = if b.brep then [Brane b] else []
let ac_res a = if a.arep then [a] else []

let phago_step bs =
  (* produces inner branes with previous siblings *)
  hole_concat_map bs
    ~f:(fun sys_hd x sys_tl ->
      match x with
      | Molecule _ -> []
      | Brane br ->
        let { actions; contents = cont_inner; _ } = br in
        hole_concat_map actions
          ~f:(fun ac_hd ac ac_tl ->
            match ac.op with
            | Phago ac_inner -> [
              { actions = ac_hd @ ac_inner @ ac_res ac @ ac_tl
              ; brep = false
              ; contents = cont_inner }, 
              sys_hd @ br_res br @ sys_tl]
            | _ -> []))
  (* match them with outer branes *)
  |> List.concat_map
    ~f:(fun (br_inner, bs') ->
      hole_concat_map bs'
        ~f:(fun sys_hd outer sys_tl ->
          match outer with 
          | Molecule _ -> []
          | Brane br_outer -> 
            let { actions; contents = cont_middle; _ } = br_outer in
            hole_concat_map actions
              ~f:(fun ac_hd ac ac_tl ->
                match ac.op with
                | CoPhago { inner = ac_middle; outer = ac_outer } -> 
                  [sys_hd @ 
                    [Brane { actions = ac_hd @ ac_outer @ ac_res ac @ ac_tl
                    ; brep = false
                    ; contents = 
                      Brane { actions = ac_middle
                      ; brep = false
                      ; contents = [Brane br_inner] }
                      :: cont_middle }]
                    @ br_res br_inner @ br_res br_outer @ sys_tl]
                | _ -> [])))

let exo_step br_outer =  
  let { actions; contents = cont_outer; _ } = br_outer in 
  hole_concat_map actions
    ~f:(fun ac_hd1 ac1 ac_tl1 -> 
      match ac1.op with
      | CoExo ac_outer1 ->
        hole_concat_map cont_outer
          ~f:(fun br_hd inner br_tl ->
            match inner with
            | Molecule _ -> []
            | Brane br_inner ->
              let { actions; contents = cont_inner; _ } = br_inner in
              hole_concat_map actions
                ~f:(fun ac_hd2 ac2 ac_tl2 ->
                  match ac2.op with
                  | Exo ac_outer2 -> [
                    Brane { actions = ac_hd2 @ ac_outer2 @ ac_res ac2 @ ac_tl2 
                      @ ac_hd1 @ ac_outer1 @ ac_res ac1 @ ac_tl1
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
        [(br_res br) @
        [Brane { actions = ac_hd @ ac_outer @ ac_res ac @ ac_tl
        ; brep = false
        ; contents = 
          Brane { actions = ac_inner
          ; brep = false
          ; contents = [] }
          :: contents }]]
      | _ -> [])

let split_match ~equal ~targets xs =
  List.fold_right xs
    ~init:([], targets) 
    ~f:(fun x (tl, targets) ->
      match List.findi ~f:(fun _ -> equal x) targets with
      | None -> x :: tl, targets
      | Some (i, _) -> tl, 
        let targets_hd, targets_tl = List.split_n targets i in
        targets_hd @ List.tl_exn targets_tl)

let%expect_test "split match fail" =
  split_match ~equal:Int.equal ~targets:[3; 3; 5] [1; 2; 3; 4; 5]
  |> Tuple2.sexp_of_t 
    (List.sexp_of_t Int.sexp_of_t)
    (List.sexp_of_t Int.sexp_of_t)
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    ((1 2 4) (3)) |}] 

let%expect_test "split match succeed" =
  split_match ~equal:Int.equal ~targets:[3; 3; 5] [3; 3; 1; 2; 3; 4; 5]
  |> Tuple2.sexp_of_t 
    (List.sexp_of_t Int.sexp_of_t)
    (List.sexp_of_t Int.sexp_of_t)
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((3 1 2 4) ()) |}]

let bind_release_step sys =
  let equal x m = 
    match x with
    | Brane _ -> false
    | Molecule m' -> equal_molecule m m'
  in
  hole_concat_map sys
    ~f:(fun sys_hd x sys_tl ->
      match x with
      | Molecule _ -> []
      | Brane b ->
        hole_concat_map b.actions
          ~f:(fun act_hd act act_tl -> 
            match act.op with
            | BindRelease { bind_out; bind_in; release_out; release_in; arg } ->
              (* Search for out molecules in both halves *)
              (match
                (match split_match ~equal ~targets:bind_out sys_tl with
                | sys_tl', [] -> Some (sys_hd, sys_tl')
                | sys_tl', bind_out' -> 
                  (match split_match ~equal ~targets:bind_out' sys_hd with
                  | sys_hd', [] -> Some (sys_hd', sys_tl')
                  | _ -> None))
              with
              | None -> []
              (* Then search for in molecules *)
              | Some (sys_hd', sys_tl') ->
                (match split_match ~equal ~targets:bind_in b.contents with
                | contents', [] -> 
                  let wrap = List.map ~f:(fun m -> Molecule m) in
                  [ wrap release_out @ sys_hd' @
                    [Brane { actions = act_hd @ arg @ ac_res act @ act_tl 
                    ; brep = false
                    ; contents = wrap release_in @ contents' 
                    }]
                    @ br_res b @ sys_tl']
                | _ -> []))
            | _ -> [])) 

let rec step_system bs =
  (* phago step *)
  phago_step bs
  (* bind release step *)
  @ bind_release_step bs
  (* recursive step *)
  @ hole_concat_map
    ~f:(fun hd x tl ->
      match x with
      | Molecule _ -> []
      | Brane b -> List.map
          ~f:(fun bs' -> hd @ 
            bs' @ tl)
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
      [Brane { actions = actions
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
  | DripTok
  | BindReleaseTok
  [@@deriving sexp, equal]

type token =
  | Id of string
  | Dot
  | Bang
  | Colon
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Comma
  | Equal
  | Let
  | In
  | RArrow
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
  else if String.equal s "drip" then Op DripTok
  else if String.equal s "exch" then Op BindReleaseTok
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
      | `Begin, Some ':' -> next (); Ok `Begin, Some Colon
      | `Begin, Some '!' -> next (); Ok `Begin, Some Bang
      | `Begin, Some '=' -> next (); Ok `Equal, None
      | `Equal, Some '>' -> next (); Ok `Begin, Some RArrow
      | `Equal, _ -> Ok `Begin, Some Equal
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
        Ok (cobud inner outer)
      | DripTok -> 
        let%bind () = eat LParen in
        let%bind inner = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind outer = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (drip inner outer)
      | BindReleaseTok ->
        let%bind () = eat LParen in
        let%bind bind_out = eat_molecules ~close:RParen in
        let%bind () = eat RParen in
        let%bind () = eat LParen in
        let%bind bind_in = eat_molecules ~close:RParen in
        let%bind () = eat RParen in
        let%bind () = eat RArrow in
        let%bind () = eat LParen in
        let%bind release_out = eat_molecules ~close:RParen in
        let%bind () = eat RParen in
        let%bind () = eat LParen in
        let%bind release_in = eat_molecules ~close:RParen in
        let%bind () = eat RParen in
        let%bind () = eat Dot in
        let%bind () = eat LParen in
        let%bind arg = eat_actions ~close:RParen act_bs in
        let%bind () = eat RParen in
        Ok (BindRelease { 
          bind_out; bind_in; release_out; release_in; arg }))
      in Ok [{ arep; op }]
    | Id action_id -> next(); begin
      match Map.find act_bs action_id with
      | Some actions -> Ok actions
      | None -> Error (NoActionForId action_id)
      end
    | token -> Error (ExpectedOp token)
  and eat_molecule () = 
    let%bind () = eat Colon in
    eat_id ()
  and eat_molecules ~close = 
    eat_many ~sep:Comma ~close eat_molecule
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
      let%bind contents = eat_sys ~close:RBracket sys_bs act_bs in
      let%bind () = eat RBracket in 
      Ok [Brane { brep; actions; contents }]
  and eat_sys ?close sys_bs act_bs = 
    let%bind sys = eat_many ~sep:Comma ?close 
      (fun () -> 
        match peek () with
        (* TODO: consistency in returns? eat_brane already has Brane ctor *)
        | Colon -> 
          let%bind m = eat_molecule () in
          Ok [Molecule m]
        | _ -> eat_brane sys_bs act_bs)
    in 
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

let string_of_molecule m = ":" ^ m 
let rec string_of_sys sys =
  string_of_list ~sep:",\n" 
    (function
    | Molecule m -> string_of_molecule m
    | Brane b -> string_of_brane b)
    sys
and string_of_brane b =
  let { actions; brep; contents } = b in 
  let s = contents |> string_of_sys |> indent in
  Printf.sprintf "%s(%s)[%s]" 
    (if brep then "!" else "")
    (actions |> string_of_actions)
    (if String.is_empty s then "" else "\n" ^ s)
and string_of_action a = 
  let { arep; op } = a in
  (if arep then "!" else "") ^
  (string_of_op op)
and string_of_actions a = string_of_list ~sep:", " string_of_action a
(* TODO: is there a better way to keep this in sync with the parser? *)
and string_of_op = function
  | Phago actions ->
    Printf.sprintf "phago.(%s)" (string_of_actions actions)
  | CoPhago { inner; outer } ->
    Printf.sprintf "cophago(%s).(%s)" 
      (string_of_actions inner)
      (string_of_actions outer)
  | Exo actions ->
    Printf.sprintf "exo.(%s)" (string_of_actions actions)
  | CoExo actions ->
    Printf.sprintf "coexo.(%s)" (string_of_actions actions)
  | Pino { inner; outer } ->
    Printf.sprintf "pino(%s).(%s)" 
      (string_of_actions inner)
      (string_of_actions outer)
  | BindRelease { bind_out; bind_in; release_out; release_in; arg } ->
    Printf.sprintf "exch(%s)(%s)=>(%s)(%s).(%s)"
      (string_of_list ~sep:", " string_of_molecule bind_out)
      (string_of_list ~sep:", " string_of_molecule bind_in)
      (string_of_list ~sep:", " string_of_molecule release_out)
      (string_of_list ~sep:", " string_of_molecule release_in)
      (string_of_actions arg)


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

let print_parse_result = function 
  | Ok sys -> print_endline (string_of_sys sys)
  | Error err -> print_endline (Sexp.to_string_hum (sexp_of_error err))

let rec left_side = function
  | Node (x, []) -> [x]
  | Node (x, hd :: _) -> x :: left_side hd

let%expect_test "let lexer test" = "
let a = mate.() in 
let b = !coexo.() in 
(a, b, exch()(:test_1)=>(:test_3)().())[:test_1, ()[], :test_2]
"
  |> sys_of_string
  |> Result.sexp_of_t sexp_of_sys sexp_of_error
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {|
    (Ok
     ((Brane
       ((actions
         (((op (Phago (((op (Exo ())) (arep false))))) (arep false))
          ((op (CoExo ())) (arep true))
          ((op
            (BindRelease (bind_out ()) (bind_in (test_1)) (release_out (test_3))
             (release_in ()) (arg ())))
           (arep false))))
        (brep false)
        (contents
         ((Molecule test_1) (Brane ((actions ()) (brep false) (contents ())))
          (Molecule test_2))))))) |}]

let%expect_test "let parser test" = 
  let s = "let a = mate.() in let b = !coexo.() in (a, b)[]" in
  print_parse_result (sys_of_string s);
  [%expect{|
    (phago.(exo.()), !coexo.())[] |}];
  [%expect {| |}]

let print_one_execution ?n s = s
  |> sys_of_string 
  |> Result.map_error 
    ~f:(fun x -> x |> sexp_of_error |> Sexp.to_string_hum)
  |> Result.ok_or_failwith
  |> eval_tree step_system (Option.value ~default:10 n) 
  |> left_side
  |> List.map ~f:(string_of_sys)
  |> List.iter ~f:(fun x -> Printf.printf "%s\n\n" x)

let%expect_test "molecular virus example" = print_one_execution "
let nucap = (!bud.())[] in
let virus = (phago.(exo.()))[nucap] in
let membrane = !cophago(). (mate. ()), !coexo. () in
let cytosol = (!comate.(), !coexo.())[] in
let cell = (membrane)[cytosol] in
let viral_envelope = cobud(phago.(exo.())).() in
let envelope_vesicle = (exo.(viral_envelope))[] in
virus, cell
";
  [%expect {|
    (phago.(exo.()))[
     (!phago.())[]],
    (!cophago().(phago.(exo.())), !coexo.())[
     (!cophago(coexo.(exo.())).(coexo.()), !coexo.())[]]

    (phago.(exo.()), !cophago().(phago.(exo.())), !coexo.())[
     ()[
      (exo.())[
       (!phago.())[]]],
     (!cophago(coexo.(exo.())).(coexo.()), !coexo.())[]] |}]

let%expect_test "basic phago" = print_one_execution "
(!phago.())[], (!cophago().())[]
";
  [%expect {|
    (!phago.())[],
    (!cophago().())[]

    (!cophago().())[
     ()[
      (!phago.())[]]] |}]

let%expect_test "basic exo" = print_one_execution "
(!coexo.())[(!exo.())[]]
";
  [%expect {|
    (!coexo.())[
     (!exo.())[]]

    (!exo.(), !coexo.())[] |}]

let%expect_test "basic pino" = print_one_execution ~n:3 "
(!pino().())[]
";
  [%expect {|
    (!pino().())[]

    (!pino().())[
     ()[]]

    (!pino().())[
     ()[],
     ()[]]

    (!pino().())[
     ()[],
     ()[],
     ()[]] |}]

let%expect_test "basic mate" = print_one_execution ~n:3 "
(mate.())[], (comate.())[]
";
  [%expect {|
    (phago.(exo.()))[],
    (cophago(coexo.(exo.())).(coexo.()))[]

    (coexo.())[
     (coexo.(exo.()))[
      (exo.())[]]]

    (coexo.())[
     (exo.())[]]

    ()[] |}]

let%expect_test "basic drip" = print_one_execution ~n:3 "
(cobud().())[(bud.())[]]
";
  [%expect {|
    (pino(cophago().(exo.())).(coexo.()))[
     (phago.())[]]

    (coexo.())[
     (cophago().(exo.()))[],
     (phago.())[]]

    (coexo.())[
     (exo.())[
      ()[
       ()[]]]]

    ()[],
    ()[
     ()[]] |}]

let%expect_test "basic drip" = print_one_execution ~n:3 "
(drip().())[]
";
  [%expect {|
    (pino(pino().(exo.())).(coexo.()))[]

    (coexo.())[
     (pino().(exo.()))[]]

    (coexo.())[
     (exo.())[
      ()[]]]

    ()[],
    ()[] |}]

let%expect_test "basic bind release" = print_one_execution "
(exch()(:a)=>()(:b).())[:a]
";
  [%expect {|
    (exch()(:a)=>()(:b).())[
     :a]

    ()[
     :b] |}]

let%expect_test "replicated bind release" = print_one_execution ~n:3 "
(exch()(:a)=>(:b)().())[
  (!exch()()=>(:a)().())[]]
";
  [%expect{|
    (exch()(:a)=>(:b)().())[
     (!exch()()=>(:a)().())[]]

    (exch()(:a)=>(:b)().())[
     :a,
     (!exch()()=>(:a)().())[]]

    :b,
    ()[
     (!exch()()=>(:a)().())[]]

    :b,
    ()[
     :a,
     (!exch()()=>(:a)().())[]] |}]

(*

*)

let%expect_test "plant vacuole" = print_one_execution "
let proton_pump = !exch(:atp)()=>(:adp, :p)(:hplus, :hminus).() in 
let ion_channel = !exch(:clminus)(:hplus)=>()(:hplus, :clminus).() in
let proton_antiporter = !exch(:naplus)(:hplus)=>(:hplus)(:naplus).() in 
let plant_vacuole = (proton_pump, ion_channel, proton_antiporter)[] in
plant_vacuole, :atp, :clminus
";
  [%expect {|
    (!exch(:atp)()=>(:adp, :p)(:hplus, :hminus).(), !exch(:clminus)(:hplus)=>()(:hplus, :clminus).(), !exch(:naplus)(:hplus)=>(:hplus)(:naplus).())[],
    :atp,
    :clminus

    :adp,
    :p,
    (!exch(:atp)()=>(:adp, :p)(:hplus, :hminus).(), !exch(:clminus)(:hplus)=>()(:hplus, :clminus).(), !exch(:naplus)(:hplus)=>(:hplus)(:naplus).())[
     :hplus,
     :hminus],
    :clminus

    :adp,
    :p,
    (!exch(:atp)()=>(:adp, :p)(:hplus, :hminus).(), !exch(:clminus)(:hplus)=>()(:hplus, :clminus).(), !exch(:naplus)(:hplus)=>(:hplus)(:naplus).())[
     :hplus,
     :clminus,
     :hminus] |}]

(* TODO: Cardelli has :trigger repeat - is this part of his syntax? *)
(* TODO: Had to inline cytosol - the contents of cell - due to parsing ambiguity *)
(* TODO: is there another execution branch? *)
let%expect_test "viral infection" = print_one_execution "
let disasm = exch(:trigger)(:vrna)=>(:vrna)().() in
let capsid = !bud.(), disasm in
let nucap = (capsid)[:vrna] in

let vrna_repl = (!exch(:vrna)()=>(:vrna, :vrna)().())[] in

let capsomers = exch(:vrna)()=>()(:vrna).(capsid) in
let capsomer_tran = (!exch(:vrna)()=>(:vrna)().(drip(capsomers).()))[] in

let viral_envelope = cobud(phago.(exo.())).() in
let envelope_vesicle = (exo.(viral_envelope))[] in

let er = (!exch(:vrna)()=>(:vrna)().(drip(exo.(viral_envelope)).()))[] in

let virus = (phago.(exo.()))[nucap] in
let membrane = !cophago(). (mate. ()), !coexo. () in
let endosome = (!comate.(), !coexo.())[] in
let cell = (membrane)[endosome, :trigger, vrna_repl, capsomer_tran, er] in

virus, cell
";
  [%expect {|
    (phago.(exo.()))[
     (!phago.(), exch(:trigger)(:vrna)=>(:vrna)().())[
      :vrna]],
    (!cophago().(phago.(exo.())), !coexo.())[
     (!cophago(coexo.(exo.())).(coexo.()), !coexo.())[],
     :trigger,
     (!exch(:vrna)()=>(:vrna, :vrna)().())[],
     (!exch(:vrna)()=>(:vrna)().(pino(pino(exch(:vrna)()=>()(:vrna).(!phago.(), exch(:trigger)(:vrna)=>(:vrna)().())).(exo.())).(coexo.())))[],
     (!exch(:vrna)()=>(:vrna)().(pino(pino(exo.(pino(cophago(phago.(exo.())).(exo.())).(coexo.()))).(exo.())).(coexo.())))[]]

    (phago.(exo.()), !cophago().(phago.(exo.())), !coexo.())[
     ()[
      (exo.())[
       (!phago.(), exch(:trigger)(:vrna)=>(:vrna)().())[
        :vrna]]],
     (!cophago(coexo.(exo.())).(coexo.()), !coexo.())[],
     :trigger,
     (!exch(:vrna)()=>(:vrna, :vrna)().())[],
     (!exch(:vrna)()=>(:vrna)().(pino(pino(exch(:vrna)()=>()(:vrna).(!phago.(), exch(:trigger)(:vrna)=>(:vrna)().())).(exo.())).(coexo.())))[],
     (!exch(:vrna)()=>(:vrna)().(pino(pino(exo.(pino(cophago(phago.(exo.())).(exo.())).(coexo.()))).(exo.())).(coexo.())))[]] |}]

(*

*)
