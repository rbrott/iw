(* interactive evaluation graph explorer *)
open Core
open Stdio

let skeletonize graph =
  let nexti = ref 0 in
  let idx_of_sys = ref (Map.empty (module Brane.Sys)) in
  let idx_of sys =
    match Map.find !idx_of_sys sys with
    | Some i -> i 
    | None -> 
      let i = !nexti in
      idx_of_sys := Map.set !idx_of_sys ~key:sys ~data:i;
      nexti := !nexti + 1;
      i
  in
  Map.fold_right graph
    ~init:(Map.empty (module Int))
    ~f:(fun ~key ~data graph ->
      let data = key, List.map ~f:idx_of data in
      let key = idx_of key in
      Map.add_exn graph ~key ~data)

let main sys =  
  let graph = sys
    |> Brane.eval_graph ~depth:17
    |> skeletonize
  in
  let rec loop i =
    (match Map.find graph i with
    | Some (sys, adj) ->
      Printf.printf "\n=================== %d ===================\n\n" i;
      let sys_str = Brane.string_of_sys sys in
        Printf.printf "%s\n\n" sys_str;
      List.iter ~f:(fun j -> 
        match Map.find graph j with
        | Some (sys', _) ->
          let sys'_str = Brane.string_of_sys sys' in
          let diff = Patdiff.Patdiff_core.patdiff
            ~prev:{ name = ""; text = sys_str } ~next:{ name = ""; text = sys'_str } () in
          Printf.printf "[%d] %s\n\n" j diff
        | None -> print_endline "not stored") adj
      (* TODO: why do I need to flush? *)
    | None ->
      Printf.printf "Invalid index %d\n" i);
    Printf.printf "> ";
    Out_channel.flush Out_channel.stdout;
    let choice_line = In_channel.input_line ~fix_win_eol:true In_channel.stdin in
    print_endline "";
    match choice_line with
    | None -> ()
    | Some line -> 
      let choice = line
        |> String.strip ~drop:(Char.equal '\n')
        |> Int.of_string
      in
      loop choice
  in loop 0

let () = "
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
let membrane = !cophago(mate.()).(), !coexo. () in
let endosome = (!comate.(), !coexo.())[] in
let cell = (membrane)[endosome, :trigger, vrna_repl, capsomer_tran, er] in

virus, cell
"
  |> Brane.sys_of_string 
  |> Result.map_error 
    ~f:(fun x -> x |> Brane.sexp_of_error |> Sexp.to_string_hum)
  |> Result.ok_or_failwith
  |> main
