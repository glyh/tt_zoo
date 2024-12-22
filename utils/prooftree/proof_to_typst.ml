open Proof_definition

let write_proof_as_typst (sink : out_channel) (proof : proof) =
  let output = Out_channel.output_string sink in
  output {|#import "@preview/curryst:0.3.0": rule, proof-tree

#proof-tree(
|};
  let output_indent indent = String.init indent (fun _ -> ' ') in
  let rec write_proof_helper indent proof =
    let { premices; conclusion; rule_name } = proof in
    match (rule_name, premices) with
    | None, [] -> output conclusion
    | Some rule_name, premices ->
        "rule(\n" |> output;
        output
          (Printf.sprintf "%sname: %s,\n"
             (output_indent (indent + 2))
             rule_name);
        output
          (Printf.sprintf "%s%s,\n" (output_indent (indent + 2)) conclusion);
        List.iter
          (fun premice ->
            output_indent (indent + 2) |> output;
            write_proof_helper (indent + 2) premice;
            ",\n" |> output)
          premices;

        output (Printf.sprintf "%s)" (output_indent indent))
    | None, premices ->
        "rule(\n" |> output;
        output
          (Printf.sprintf "%s%s,\n" (output_indent (indent + 2)) conclusion);
        List.iter
          (fun premice ->
            output_indent (indent + 2) |> output;
            write_proof_helper (indent + 2) premice;
            output ",\n")
          premices;

        output (Printf.sprintf "%s)" (output_indent indent))
  in
  output "  ";
  write_proof_helper 2 proof;
  output "\n)"
