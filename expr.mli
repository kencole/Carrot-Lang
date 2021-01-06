type operator =
  | O_plus
  | O_minus
  | O_times
  | O_divide
[@@deriving sexp]
 ;;

type t =
  | E_num of int
  | E_str of string
  | E_symb of string
  | E_op of operator * (t list)
  | E_app of t list
[@@deriving sexp]
;;
(*
val find_matching_close : string -> int -> int option

val get_toplevel_statements : string -> string list

val get_statement_tokens : string -> string list

val parse_operator : string -> operator option

val parse_literal : string -> expr

val parse_statement : string -> expr
*)

val parse_file_body : string -> t list
