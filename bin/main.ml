open Sy

let usage =
  "sy — SY reference implementation (Milestone 1)\n\n\
   usage:\n\
  \  sy run   FILE.sy    type-check, then run; prints program output and the\n\
  \                      value of each top-level expression\n\
  \  sy check FILE.sy    type- and effect-check only; prints the inferred type\n\
  \                      of every definition and expression\n\
  \  sy core  FILE.sy    print the program lowered to the seven core forms\n"

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error m ->
    prerr_endline ("sy: " ^ m);
    exit 2

let fail file (pos : Sexp.pos) msg =
  Printf.eprintf "%s:%d:%d: %s\n" file pos.line pos.col msg;
  exit 1

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ ("run" | "check") as cmd; file ] -> (
      let mode = if cmd = "run" then Driver.Run else Driver.Check in
      let s = Driver.make_session () in
      try Driver.run_source s ~mode (read_file file)
      with Driver.Sy_error (pos, msg) -> flush stdout; fail file pos msg)
  | [ "core"; file ] -> (
      try
        List.iter (fun (_, d) -> print_endline (Pretty.decl d)) (Parse.program (read_file file))
      with Sexp.Syntax_error (pos, msg) -> fail file pos ("syntax error: " ^ msg))
  | [ ("-h" | "--help" | "help") ] | [] -> print_string usage
  | _ ->
      prerr_string usage;
      exit 2
