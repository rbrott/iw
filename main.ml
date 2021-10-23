open Core

open Brane

let () =
  let s = Replicate (Replicate (Compose (Empty, Membrane ("Test", Empty)))) in
  Out_channel.output_string 
    (Out_channel.create ~fail_if_exists:false "main.dot")
    ("graph { " ^ (snd (dot_of_system 0 s)) ^ "}")
