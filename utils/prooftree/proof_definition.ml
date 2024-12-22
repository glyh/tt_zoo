type statement = string

type proof = {
  premices : proof list;
  conclusion : statement;
  rule_name : string option;
}
