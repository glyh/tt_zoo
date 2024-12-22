open Stlc_with_proof_lib.Parser
open Stlc_with_proof_lib.Typecheck
open Prooftree.Proof_to_typst

let () =
  match Array.length Sys.argv with
  | 3 ->
      let source_file = Sys.argv.(1) in
      let proof_file = Sys.argv.(2) in
      let ast = parse_file source_file in
      let _, proof = infer BatMap.empty ast in
      let oc = open_out proof_file in
      write_proof_as_typst oc proof;
      close_out oc
  | _ ->
      let exe_name = Sys.argv.(0) in
      Printf.printf "USAGE: %s [SOURCE] [PROOFTREE]" exe_name
