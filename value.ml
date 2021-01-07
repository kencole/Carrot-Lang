open Core;;

type t =
  | V_none
  | V_num of int
  | V_str of string
  | V_builtin of string
[@@deriving sexp]

type environment = stored_value String.Map.t
and stored_value =
  | Builtin of (t list -> environment -> t * environment)
  | Variable of t

let apply_on_two f =
  let func l env =
    match l with
    | [a ; b] -> f a b env
    | _ -> Error.raise (Error.of_string "Awef")
  in func
;;

let get_starter_env () =
  let define sym v env =
    match sym with
    | V_str s ->
      (let new_env =
         Map.set ~key:s ~data:(Variable v) env
       in
       V_none, new_env)
    | _ -> (V_none, env)
  in
  let define = apply_on_two define in
    String.Map.of_alist_exn [
    "define", Builtin define
  ]


let val_to_ints args =
    let val_to_int = function
    | V_num v -> v
    | _ -> 0 (* error here? *)
  in
  List.map ~f:val_to_int args
;;

let vals_to_ints_then_fold f init args =
  val_to_ints args
  |> List.fold ~init ~f
;;

exception Error of string

let fold_with_first_as_init ~f args =
  match args with
  | [] -> raise (Error "Empty list") (*throw error here *)
  | h :: t -> List.fold ~f ~init:h t
;;

let vals_to_ints_then_fold_with_first_as_init f args =
  val_to_ints args
  |> fold_with_first_as_init ~f


let rec eval (expr : Expr.t) ~env : t * environment =
  match expr with
  | E_num n -> V_num n, env
  | E_str s -> V_str s, env
  | E_op (op, args) -> eval_op op args ~env
  | E_app args ->
    eval_app args ~env
  | E_symb s -> match Map.find env s with
    | None -> V_str s, env (* throw error here *)
    | Some sv ->
      (match sv with
       | Variable v -> v, env
       | Builtin _func -> V_builtin s, env) (* ????? *)
and eval_op (op : Expr.operator) args ~env =
  let args =
    List.map ~f:(eval ~env) args
    |> List.map ~f:fst
  in
  let v = match op with
    | O_plus -> V_num (vals_to_ints_then_fold Int.(+) 0 args)
    | O_times -> V_num (vals_to_ints_then_fold Int.( * ) 1 args)
    | O_minus ->
      V_num (vals_to_ints_then_fold_with_first_as_init Int.(-) args)
   | O_divide ->
     V_num (vals_to_ints_then_fold_with_first_as_init Int.(/) args)
   in
   (v, env)
and eval_app args ~env =
  let args =
    List.map ~f:(eval ~env) args
    |> List.map ~f:fst
  in
  match args with
  | [] -> V_none, env (* throw error here *)
  | hd :: tl ->
    match hd with
    | V_builtin func ->
      (match Map.find_exn env func with
      | Variable _ -> raise (Error "awef")
      | Builtin func -> func tl env)
    | _ -> V_none, env (* TODO *)
;;

let eval_expr_with_env (expr : Expr.t) (env : environment) : t * environment =
  eval expr ~env
;;

let eval_expr (expr : Expr.t) : t * environment =
  let starter_env = get_starter_env () in
  eval_expr_with_env expr starter_env
;;



let print_int i =
  Int.to_string i
  |> Out_channel.output_string stdout
;;

let print_int_newline i =
  print_int i;
  print_string "\n"
;;

let print_newline s =
  print_string s;
  print_string "\n"
;;

let print_value = function
  | V_num n -> print_int_newline n
  | V_str s -> print_newline s
  | V_none -> print_newline "None"
  | V_builtin s ->
    print_string "<builtin: ";
    print_string s;
    print_newline ">"


(* BEGIN tests ----------------------------------------------------- *)


