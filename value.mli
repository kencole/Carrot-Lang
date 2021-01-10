type vlist =
  | Empty
  | Cons of t * vlist
and t =
  | V_none
  | V_num of int
  | V_str of string
  | V_bool of bool
  | V_builtin of string
  | V_list of vlist
  | V_fun of string list * Expr.t * environment
[@@deriving sexp]

and environment = stored_value Core.String.Map.t
and stored_value =
  | Builtin of (t list -> environment -> t * environment)
  | Variable of t
  | Recursive of t ref

val eval_expr : Expr.t -> t * environment

val eval_expr_with_env : Expr.t -> environment -> t * environment

val print_value : t -> unit
