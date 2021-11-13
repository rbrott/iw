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

let () = Examples.enzyme_mutex
  |> Brane.sys_of_string 
  |> Result.map_error 
    ~f:(fun x -> x |> Brane.sexp_of_error |> Sexp.to_string_hum)
  |> Result.ok_or_failwith
  |> main
