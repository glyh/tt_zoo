open Proof_definition
open Proof_to_typst

let%test "typst output" =
  write_proof_as_typst Out_channel.stdout
    {
      premices =
        [
          {
            premices =
              [
                {
                  premices =
                    [
                      { premices = []; rule_name = None; conclusion = "$Pi_1$" };
                    ];
                  rule_name = None;
                  conclusion = "$C_1 or L$";
                };
              ];
            rule_name = Some "$A$";
            conclusion = "$C_1 or C_2 or L$";
          };
          {
            premices =
              [ { premices = []; rule_name = None; conclusion = "$Pi_2$" } ];
            rule_name = None;
            conclusion = "$C_2 or overline(L)$";
          };
        ];
      rule_name = Some "$R$";
      conclusion = "$C_1 or C_2 or C_3 $";
    };

  true
