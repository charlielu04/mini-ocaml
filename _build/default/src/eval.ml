open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(* type values = Int of int|Bool of bool|String of string *)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value v ->
    v
  | ID id ->
    ref_lookup env id
  | Not expr ->
    (match eval_expr env expr with
    | Bool b ->
      Bool(not b)
    | _ ->
      raise(TypeError("Used not on non boolean type")))

  | Binop (op, expr1, expr2) ->
    (match op with 
    | Add ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Int(i1 + i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )

    | Sub ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Int(i1 - i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )

    | Mult -> 
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Int(i1 * i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )

    | Div ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        if i1 = 0 then 
          raise(DivByZeroError)
        else if i2 = 0 then
          raise(DivByZeroError)
        else
          Int(i1 / i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )
    | Greater ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 > i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )
    | Less ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 < i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )
    | GreaterEqual ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 >= i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )
    | LessEqual ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 <= i2)
      | Int i1, _ ->
        raise(TypeError("val2 is not int"))
      | _, Int i2 ->
        raise(TypeError("val1 is not int"))
      | _, _ ->
        raise(TypeError("val1 and val2 are not ints"))
      )
    | Concat ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | String s1, String s2 ->
        String(s1 ^ s2)
      | String s1, _ ->
        raise(TypeError("s2 not string"))
      | _, String s2 ->
        raise(TypeError("s1 not string"))
      | _, _ ->
        raise(TypeError("s1 and s2 not string"))
      )
    | Equal ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 = i2)
      | Bool b1, Bool b2 ->
        Bool(b1 = b2)
      | String s1, String s2 ->
        Bool(String.equal s1 s2)
      | _, _ -> 
        raise(TypeError("wrong types for equal"))
      )
    | NotEqual ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Int i1, Int i2 ->
        Bool(i1 <> i2)
      | Bool b1, Bool b2 ->
        Bool(b1 <> b2)
      | String s1, String s2 ->
        Bool(not(String.equal s1 s2))
      | _, _ -> 
        raise(TypeError("wrong types for notequal"))
      )
    | Or ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Bool b1, Bool b2 ->
        Bool(b1 || b2)
      | _, _ ->
        raise(TypeError("Not bools"))
      )
    | And ->
      let val1 = eval_expr env expr1 in
      let val2 = eval_expr env expr2 in

      (match val1, val2 with
      | Bool b1, Bool b2 ->
        Bool(b1 && b2)
      | _, _ ->
        raise(TypeError("Not bools"))
      )
    )
  | If (expr1, expr2, expr3) ->
    let val1 = eval_expr env expr1 in
    (match val1 with
    | Bool b ->
      if b then
        eval_expr env expr2
      else
        eval_expr env expr3
    | _ ->
      raise(TypeError("expr1 not bool")))
  | Let (idname, boolean, init, body) ->
    (match boolean with
    | true -> 
      let temp_env = ref_extend_tmp env idname in
      let v = eval_expr temp_env init in 
      ref_update temp_env idname v;
      eval_expr temp_env body
    | false ->
      let v = eval_expr env init in
      let updated_env = ref_extend env idname v in
      eval_expr updated_env body
    )
  | Fun (parameter, body) ->
    Closure(env, parameter, body)
  | FunctionCall (expr1, expr2) ->
    let val1 = eval_expr env expr1 in
    
    (match val1 with
    | Closure (envA, x, e) ->
      let v = eval_expr env expr2 in
      let new_env = ref_extend envA x v in
      eval_expr new_env e
    | _ ->
      raise(TypeError("Not closure"))
    )



(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def (var, expr) ->
    let temp_env = ref_extend_tmp env var in
    let v = eval_expr temp_env expr in
    ref_update temp_env var v;
    (temp_env, Some v)
  | Expr expr ->
    (env, Some (eval_expr env expr))
  | NoOp ->
    (env, None)