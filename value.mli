type t =
  | V_none
  | V_num of int
  | V_str of string
  | V_builtin of string
[@@deriving sexp]

type environment = stored_value Core.String.Map.t
and stored_value =
  | Builtin of (t list -> environment -> t * environment)
  | Variable of t


val eval_expr : Expr.t -> t * environment

val eval_expr_with_env : Expr.t -> environment -> t * environment

val print_value : t -> unit
