(* interactive evaluation tree explorer *)
open Core
open Stdio

let main sys =  
  let rec loop sys =
    let sys_str = Brane.string_of_sys sys in
    let next = Brane.step_system sys in
      Printf.printf "%s\n\n" sys_str;
    List.iteri ~f:(fun i sys' -> 
      let sys'_str = Brane.string_of_sys sys' in
      let diff = Patdiff.Patdiff_core.patdiff
        ~prev:{ name = ""; text = sys_str } ~next:{ name = ""; text = sys'_str } () in
      Printf.printf "[%d] %s\n\n" i diff) next;
    (* TODO: why do I need to flush? *)
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
      loop (List.nth_exn next choice)
  in loop sys

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
