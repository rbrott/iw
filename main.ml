open Core

open Brane

let () =
  let s = Replicate (Replicate (Compose (Empty, Membrane ("Test", Empty)))) in
  (* let s = Compose (Empty, Compose (Empty, Empty)) in
  let t = eval_tree step_system 10 s in
  let _, dot = dot_of_tree 
    (fun i s -> 
      let i, s = dot_of_system i s in
      i + 1, Printf.sprintf "subgraph cluster_%d { style=bold; dummy_%d[shape=point,style=invis] %s }" i i s)
    0 t in *)
  let _, dot = dot_of_system 0 s in
  Out_channel.output_string 
    (Out_channel.create ~fail_if_exists:false "main.dot")
    ("digraph { compound=true; " ^ dot ^ " }")

