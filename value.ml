open Core;;

type vlist =
  | Empty
  | Cons of t * vlist
and t =
  | V_none
  | V_num of int
  | V_str of string
  | V_builtin of string
  | V_list of vlist
[@@deriving sexp]

type environment = stored_value String.Map.t
and stored_value =
  | Builtin of (t list -> environment -> t * environment)
  | Variable of t

let print_newline s =
  print_string s;
  print_string "\n"
;;

let to_string = function
  | V_num n -> Int.to_string n
  | V_str s -> s
  | V_none -> "None"
  | V_builtin s -> "<builtin: " ^ s ^ ">"
  | V_list lis ->
    Sexp.to_string [%sexp (lis : vlist)]
    
let print_value t =
  print_newline (to_string t)
      
let apply_on_two f =
  let func l env =
    match l with
    | [a ; b] -> f a b env
    | _ ->
      let error_string =
        "Incorrect number of arguments. Expected 2 got "
        ^ (Int.to_string (List.length l))
      in
      Error.raise (Error.of_string error_string)
  in func
;;

let get_starter_env () =
  let starter_env =
    ["none", Variable V_none]
  in
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
  let starter_env =
    ("define", Builtin define) :: starter_env
  in
  let prnt args env =
    let print_one t =
      print_string (to_string t)
    in
    let print_one_then_space v =
      print_one v;
      print_string " "
    in
    List.iter ~f:print_one_then_space args;
    print_newline "";
    (V_none, env)
  in
  let starter_env =
    ("print", Builtin prnt) :: starter_env
  in
  let cons (f : t) (r : t) env =
    match r with
    | V_list lis -> (V_list (Cons (f, lis)), env)
    | _ -> Error.raise (Error.of_string "Expected list type")
  in
  let cons = apply_on_two cons in
  let starter_env =
    ("cons", Builtin cons) :: starter_env
  in
  let empty _args env =
    (V_list Empty, env)
  in
  let starter_env =
    ("empty", Builtin empty) :: starter_env
  in
  String.Map.of_alist_exn starter_env
;;


let val_to_ints args =
    let val_to_int = function
    | V_num v -> v
    | _ as v ->
      Error.raise
        (Error.of_string ("Expected int received" ^ (to_string v)))
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
  | [] -> raise (Error "No args") 
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
    | None ->
      let error_string =
        "Unbound identifier " ^ s
      in
      Error.raise (Error.of_string error_string)
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
  (* special case to not error on unbound identifiers for define *)
  let (args : Expr.t list) =
    match args with
    | (E_symb "define") :: (E_symb unbound) :: r ->
      (E_symb "define") :: (E_str unbound) :: r
    | (E_symb "define") :: (E_str s) :: _ ->
      Error.raise
        (Error.of_string ("Expected symbol - found \"" ^ s ^ "\""))
    | _ -> args
   in   
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



(* BEGIN tests ----------------------------------------------------- *)


