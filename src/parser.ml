open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let get_id tok =
  match tok with 
  | Some (Tok_ID str) ->
    str
  | _ ->
    raise(InvalidInputException("error in get_id"))

let rec parse_expr toks = 
  let head_tok = lookahead toks in
  match head_tok with
  | Some Tok_Let -> parse_let toks
  | Some Tok_Fun -> parse_fun toks
  | Some Tok_If -> parse_if toks
  | _ -> parse_or toks

  and parse_let toks = 
    let toks_after_let = match_token toks Tok_Let in
    match lookahead toks_after_let with 
    | Some Tok_Rec -> 
      let toks_after_rec = match_token toks_after_let Tok_Rec in
      let tok_id_string = get_id (lookahead toks_after_rec) in
      let toks_after_id = match_token toks_after_rec (Tok_ID tok_id_string) in 
      let toks_after_eq = match_token toks_after_id Tok_Equal in
      let (toks_after_expr1, expr1) = parse_expr toks_after_eq in
      let toks_after_in = match_token toks_after_expr1 Tok_In in
      let (toks_after_expr2, expr2) = parse_expr toks_after_in in
      (toks_after_expr2, Let(tok_id_string, true, expr1, expr2))
    | Some Tok_ID tok_id_string ->
      let toks_after_id = match_token toks_after_let (Tok_ID tok_id_string) in
      let toks_after_eq = match_token toks_after_id Tok_Equal in
      let (toks_after_expr1, expr1) = parse_expr toks_after_eq in
      let toks_after_in = match_token toks_after_expr1 Tok_In in
      let (toks_after_expr2, expr2) = parse_expr toks_after_in in
      (toks_after_expr2, Let(tok_id_string, false, expr1, expr2))
    | _ -> raise (InvalidInputException("parse_let fail"))
    
  and parse_fun toks = 
    let toks_after_fun = match_token toks Tok_Fun in
    let tok_id_string = get_id (lookahead toks_after_fun) in
    let toks_after_id = match_token toks_after_fun (Tok_ID tok_id_string) in
    let toks_after_arrow = match_token toks_after_id Tok_Arrow in
    let (toks_after_expr, expr) = parse_expr toks_after_arrow in
    (toks_after_expr, Fun(tok_id_string, expr))

  and parse_if toks =
    let toks_after_if = match_token toks Tok_If in
    let (toks_after_expr1, expr1) = parse_expr toks_after_if in
    let toks_after_then = match_token toks_after_expr1 Tok_Then in
    let (toks_after_expr2, expr2) = parse_expr toks_after_then in
    let toks_after_else = match_token toks_after_expr2 Tok_Else in
    let (toks_after_expr3, expr3) = parse_expr toks_after_else in
    (toks_after_expr3, If(expr1, expr2, expr3))

  and parse_or toks =
    let (toks_after_and, and_expr) = parse_and toks in
    match lookahead toks_after_and with
    | Some Tok_Or ->
      let toks_after_or_op = match_token toks_after_and Tok_Or in
      let (toks_after_or, or_expr) = parse_or toks_after_or_op in
      (toks_after_or, Binop(Or, and_expr, or_expr))
    | _ ->
      (toks_after_and, and_expr)

  and parse_and toks =
    let (toks_after_equality, equality_expr) = parse_equality toks in
    match lookahead toks_after_equality with
    | Some Tok_And ->
      let toks_after_and_op = match_token toks_after_equality Tok_And in
      let (toks_after_and, and_expr) = parse_and toks_after_and_op in
      (toks_after_and, Binop(And, equality_expr, and_expr))
    | _ ->
      (toks_after_equality, equality_expr)

  and parse_equality toks =
    let (toks_after_rel, rel_expr) = parse_rel toks in
    match lookahead toks_after_rel with
    | Some Tok_Equal ->
      let toks_after_eq_op = match_token toks_after_rel Tok_Equal in
      let (toks_after_equality, equality_expr) = parse_equality toks_after_eq_op in
      (toks_after_equality, Binop(Equal, rel_expr, equality_expr))
    | Some Tok_NotEqual ->
      let toks_after_neq_op = match_token toks_after_rel Tok_NotEqual in
      let (toks_after_noteq, noteq_expr) = parse_equality toks_after_neq_op in
      (toks_after_noteq, Binop(NotEqual, rel_expr, noteq_expr))
    | _ ->
      (toks_after_rel, rel_expr)

  and parse_rel toks =
    let (toks_after_add, add_expr) = parse_add toks in
    match lookahead toks_after_add with
    | Some Tok_Less -> 
      let toks_after_less = match_token toks_after_add Tok_Less in
      let (toks_after_rel_expr, rel_expr) = parse_rel toks_after_less in
      (toks_after_rel_expr, Binop(Less, add_expr, rel_expr))
    | Some Tok_Greater -> 
      let toks_after_greater = match_token toks_after_add Tok_Greater in
      let (toks_after_rel_expr, rel_expr) = parse_rel toks_after_greater in
      (toks_after_rel_expr, Binop(Greater, add_expr, rel_expr))
    | Some Tok_LessEqual ->
      let toks_after_lesseq = match_token toks_after_add Tok_LessEqual in
      let (toks_after_rel_expr, rel_expr) = parse_rel toks_after_lesseq in
      (toks_after_rel_expr, Binop(LessEqual, add_expr, rel_expr))
    | Some Tok_GreaterEqual ->
      let toks_after_greatereq = match_token toks_after_add Tok_GreaterEqual in
      let (toks_after_rel_expr, rel_expr) = parse_rel toks_after_greatereq in
      (toks_after_rel_expr, Binop(GreaterEqual, add_expr, rel_expr))
    | _ ->
      (toks_after_add, add_expr)
    
  and parse_add toks =
    let (toks_after_mult, mult_expr) = parse_mult toks in
    match lookahead toks_after_mult with
    | Some Tok_Add ->
      let toks_after_add_op = match_token toks_after_mult Tok_Add in
      let (toks_after_add, add_expr) = parse_add toks_after_add_op in
      (toks_after_add, Binop(Add, mult_expr, add_expr))
    | Some Tok_Sub ->
      let toks_after_sub_op = match_token toks_after_mult Tok_Sub in
      let (toks_after_add, add_expr) = parse_add toks_after_sub_op in
      (toks_after_add, Binop(Sub, mult_expr, add_expr))
    | _ ->
      (toks_after_mult, mult_expr)

  and parse_mult toks =
    let (toks_after_concat, concat_expr) = parse_concat toks in
    match lookahead toks_after_concat with
    | Some Tok_Mult ->
      let toks_after_mult_op = match_token toks_after_concat Tok_Mult in
      let (toks_after_mult_expr, mult_expr) = parse_mult toks_after_mult_op in
      (toks_after_mult_expr, Binop(Mult, concat_expr, mult_expr))
    | Some Tok_Div ->
      let toks_after_mult_op = match_token toks_after_concat Tok_Div in
      let (toks_after_mult_expr, mult_expr) = parse_mult toks_after_mult_op in
      (toks_after_mult_expr, Binop(Div, concat_expr, mult_expr))
    | _ -> (toks_after_concat, concat_expr)

  and parse_concat toks =
    let (toks_after_unary, unary_expr) = parse_unary toks in
    match lookahead toks_after_unary with
    | Some Tok_Concat ->
      let toks_after_concat_op = match_token toks_after_unary Tok_Concat in
      let (toks_after_concat_expr, concat_expr) = parse_concat toks_after_concat_op in
      (toks_after_concat_expr, Binop(Concat, unary_expr, concat_expr))
    | _ ->
      (toks_after_unary, unary_expr)

  and parse_unary toks = 
    match lookahead toks with
    | Some Tok_Not -> 
      let toks_after_not = match_token toks Tok_Not in
      let (toks_after_unary, unary_expr) = parse_unary toks_after_not in
      (toks_after_unary, Not(unary_expr))
    | _ ->
      parse_funcall toks
  
  and parse_funcall toks =
    let (toks_after_primary_expr1, primary_expr1) = parse_primary toks in
    match lookahead toks_after_primary_expr1 with
    | Some Tok_Int integer ->
      let (toks_after_primary_expr2, primary_expr2) = parse_primary toks_after_primary_expr1 in
      (toks_after_primary_expr2, FunctionCall(primary_expr1, primary_expr2))
    | Some Tok_Bool boolean ->
      let (toks_after_primary_expr2, primary_expr2) = parse_primary toks_after_primary_expr1 in
      (toks_after_primary_expr2, FunctionCall(primary_expr1, primary_expr2))
    | Some Tok_String str ->
      let (toks_after_primary_expr2, primary_expr2) = parse_primary toks_after_primary_expr1 in
      (toks_after_primary_expr2, FunctionCall(primary_expr1, primary_expr2))
    | Some Tok_ID id ->
      let (toks_after_primary_expr2, primary_expr2) = parse_primary toks_after_primary_expr1 in
      (toks_after_primary_expr2, FunctionCall(primary_expr1, primary_expr2))
    | Some Tok_LParen ->
      let (toks_after_primary_expr2, primary_expr2) = parse_primary toks_after_primary_expr1 in
      (toks_after_primary_expr2, FunctionCall(primary_expr1, primary_expr2))
    | _ ->
      (toks_after_primary_expr1, primary_expr1)

  and parse_primary toks =
    match lookahead toks with
    | Some Tok_Int integer ->
      let toks_after_int = match_token toks (Tok_Int integer) in
      (toks_after_int, Value(Int integer))
    | Some Tok_Bool boolean ->
      let toks_after_bool = match_token toks (Tok_Bool boolean) in
      (toks_after_bool, Value(Bool boolean))
    | Some Tok_String s ->
      let toks_after_string = match_token toks (Tok_String s) in
      (toks_after_string, Value(String s))
    | Some Tok_ID id ->
      let toks_after_id = match_token toks (Tok_ID id) in
      (toks_after_id, ID(id))
    | Some Tok_LParen ->
      let toks_after_lparen = match_token toks Tok_LParen in
      let (toks_after_expr, expr) = parse_expr toks_after_lparen in
      let toks_after_rparen = match_token toks_after_expr Tok_RParen in
      (toks_after_rparen, expr)
    | _ -> raise(InvalidInputException("error in parse_primary"))




(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  let head_tok = lookahead toks in
  match head_tok with
  | Some Tok_Def ->
    parse_defmutop toks
  | Some Tok_DoubleSemi ->
    let toks_after_double_semi = match_token toks Tok_DoubleSemi in
    (toks_after_double_semi, NoOp)
  | _ -> parse_exprmutop toks

and parse_defmutop toks =
  let toks_after_def = match_token toks Tok_Def in
  let tok_id_string = get_id (lookahead toks_after_def) in
  let toks_after_eq = match_many toks_after_def [(Tok_ID tok_id_string); Tok_Equal] in
  let (toks_after_expr, expr) = parse_expr toks_after_eq in
  let toks_after_double_semi = match_token toks_after_expr Tok_DoubleSemi in
  (toks_after_double_semi, Def(tok_id_string, expr))

and parse_exprmutop toks =
  let (toks_after_expr, expr) = parse_expr toks in
  let toks_after_double_semi = match_token toks_after_expr Tok_DoubleSemi in
  (toks_after_double_semi, Expr expr)