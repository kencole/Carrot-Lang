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

val parse_file_body : string -> t list
