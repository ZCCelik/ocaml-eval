type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

type env = (string * int) list

let rec eval (e : expr) (env : env) : int = match e with
  | Int n -> n
  | Var x -> List.assoc x env
  | Add (e1, e2) -> (eval e1 env) + (eval e2 env)
  | Sub (e1, e2) -> (eval e1 env) - (eval e2 env)
  | Let (x, e1, e2) ->
      let value = eval e1 env in
      eval e2 ((x, value) :: env)
  | If (cond, e_then, e_else) ->
      if eval cond env <> 0 then eval e_then env else eval e_else env
