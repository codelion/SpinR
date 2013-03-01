(* set up for command argument
   using Sys and Arg modules *)
let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"
let file = ref ""

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse [] (fun s -> file := s) usage; 
  if String.length !file == 0 then print_endline usage 
  else 
    let _ = print_endline "Loading Promela Model ..." in
    let (s,p) = Parser.parse_file !file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline ("  as ") in
		let _ = (Promela.Model.to_channel stdout p) in
    let _ = print_endline "Refining to C ..." in
	  let c = Promela.Refine.translate_model p in
	  let s = (Promela.Refine.string_of_prog c) in
		let _ = print_endline s in
	  let oc = open_out ((!file)^".c") in
	  output_string oc s