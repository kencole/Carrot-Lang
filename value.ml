open Core;;

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
and environment = stored_value String.Map.t
and stored_value =
  | Builtin of (t list -> environment -> t * environment)
  | Variable of t
  | Recursive of t ref

let print_newline s =
  print_string s;
  print_string "\n"
;;

let rec to_string = function
  | V_num n -> Int.to_string n
  | V_str s -> s
  | V_none -> "None"
  | V_bool b -> if b then "true" else "false" 
  | V_builtin s -> "<builtin: " ^ s ^ ">"
  | V_list lis ->
    let rec build_list t acc =
      match t with
      | Empty -> acc
      | Cons (t, lis) ->
        build_list lis ((to_string t) :: ", " :: acc)
    in
    let pieces =
      let
        head = "[" :: (build_list lis [])
      in
      (match List.length head with
       | 1 -> ["[]"]
       | _ as len ->
         let pieces = 
           List.take head (len - 1)
         in
         List.append pieces ["]"])
    in                  
    let buf = Buffer.create 16 in
    List.iter ~f:(Buffer.add_string buf) pieces;
    Buffer.contents buf          
  | V_fun (_param, _expr, _env) -> "<function>"
    
let print_value t =
  print_newline (to_string t)

let _apply_on_three f =
  let func l env =
    match l with
    | [a ; b; c] -> f a b c env
    | _ ->
      let error_string =
        "Incorrect number of arguments. Expected 3 got "
        ^ (Int.to_string (List.length l))
      in
      Error.raise (Error.of_string error_string)
  in func
;;

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

let apply_on_one f =
  let func l env =
    match l with
    | [a] -> f a env
    | _ ->
      let error_string =
        "Incorrect number of arguments. Expected 1 got "
        ^ (Int.to_string (List.length l))
      in
      Error.raise (Error.of_string error_string)
  in func
;;


let get_starter_env () =
  let starter_env =
    ["none", Variable V_none;
     "true", Variable (V_bool true);
     "false", Variable (V_bool false);
     "empty", Variable (V_list Empty)]
  in
  let bool_and =
    let bool_and a b env =
      let bool_and =
        (match a, b with
         | V_bool true, V_bool true -> V_bool true
         | V_bool _, V_bool _ -> V_bool false
         | _ -> Error.raise (Error.of_string "and got non bool"))
      in
      bool_and, env
    in
    apply_on_two bool_and
  in
  let starter_env =
    ("and", Builtin bool_and) :: starter_env
  in
  let bool_or =
    let bool_or a b env =
      let bool_or =
        (match a, b with
         | V_bool true, V_bool _
         | V_bool _, V_bool true -> V_bool true
         | V_bool _, V_bool _ -> V_bool false
         | _ -> Error.raise (Error.of_string "or got non bool"))
      in
      bool_or, env
    in
    apply_on_two bool_or
  in
  let starter_env =
    ("or", Builtin bool_or) :: starter_env
  in
  let equals a b env =
    let eq = match a, b with
      | V_num a, V_num b -> V_bool (a = b)
      | V_bool a, V_bool b -> V_bool Bool.(a = b)
      | V_str a, V_str b -> V_bool String.(a = b)
      | _ -> V_bool false
    in
    eq, env
  in
  let equals = apply_on_two equals in
  let starter_env =
    ("=", Builtin equals) :: starter_env
  in
  let define sym v env =
    match sym with
    | V_str s ->
      (let new_env =
         Map.set ~key:s ~data:(Variable v) env
       in
       V_none, new_env)
    | _ -> (V_none, env) (* error here *)
  in
  let define = apply_on_two define in
  let starter_env =
    ("define", Builtin define) :: starter_env
  in
  let prnt args env =
    let print_one t =
      print_string (to_string t);
    in
    let print_one_then_space v =
      print_one v;
      print_string " "
    in
    List.iter ~f:print_one_then_space args;
    print_newline "";
    Out_channel.flush stdout;
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
  let is_empty arg env =
    match arg with
    | V_list Empty -> (V_bool true, env)
    | V_list _ -> (V_bool false, env)
    | _ -> Error.raise (Error.of_string "is_empty got non-list")
  in
  let is_empty = apply_on_one is_empty in
  let starter_env =
    ("empty?", Builtin is_empty) :: starter_env
  in
  let first arg env =
    match arg with
    | V_list (Cons (f, _r)) -> (f, env)
    | V_list  _ -> Error.raise (Error.of_string "first called on empty")
    | _ -> Error.raise (Error.of_string "first got non-list")
  in
  let first = apply_on_one first in
  let starter_env =
    ("first", Builtin first) :: starter_env
  in
  let rest arg env =
    match arg with
    | V_list (Cons (_f, r)) -> (V_list r, env)
    | V_list  _ -> Error.raise (Error.of_string "rest called on empty")
    | _ -> Error.raise (Error.of_string "rest got non-list")
  in
  let rest = apply_on_one rest in
  let starter_env =
    ("rest", Builtin rest) :: starter_env
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
       | Recursive v -> !v, env
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
  match args with
  | (E_symb "begin") :: r ->
    (let pass_along_env_func env expr =
       let v, env = eval expr ~env in
       env, v
     in
     let last =
       List.folding_map r ~f:pass_along_env_func ~init:env
      |> List.last
     in
     match last with
     | Some v -> v, env
     | None -> V_none, env)
  | [(E_symb "let") ; (E_symb unbound) ; v ; body] ->
    let new_env =
      Map.set ~key:unbound ~data:(Variable (fst (eval v ~env))) env
    in
    eval body ~env:new_env
  | [(E_symb "lambda") ; (E_symb param) ; body] ->
    V_fun ([param], body, env), env
  | (E_symb "deffun*") :: (E_symb unbound) :: params ->
    let body = List.last_exn params in
    let params = List.take params ((List.length params) - 1)
                 |> List.map ~f:(fun e ->
                     match e with
                     | E_symb s -> s
                     | _ -> Error.raise
                              (Error.of_string ("parameter not symbol")))
    in
    let deffunrec sym params body env =
      let func =
        let inner_env =
          let empty_func =
            V_fun ([], E_num 0, String.Map.of_alist_exn [])
          in
          Map.set ~key:sym ~data:(Recursive (ref empty_func)) env
        in
        V_fun (params, body, inner_env)
      in
      let new_env =
         Map.set ~key:sym ~data:(Variable func) env
      in
      (match func with
        | V_fun (_, _, env) ->
          (match Map.find_exn env sym with
           | Recursive r -> r := func
           | _ -> ())
        | _ -> ());
      V_none, new_env
    in
    deffunrec unbound params body env    
  | (E_symb "deffun") :: (E_symb unbound) :: params ->
    let body = List.last_exn params in
    let params = List.take params ((List.length params) - 1)
                 |> List.map ~f:(fun e ->
                     match e with
                     | E_symb s -> s
                     | _ -> Error.raise
                              (Error.of_string ("parameter not symbol")))
    in
    let deffun sym params body env =
      let func =
        V_fun (params, body, env)
      in
      let new_env =
         Map.set ~key:sym ~data:(Variable func) env
      in
      V_none, new_env
    in
    deffun unbound params body env
  | [(E_symb "if") ; cond ; cons ; altern] ->
    (match fst (eval cond ~env) with
    | V_bool true -> eval cons ~env
    | V_bool false -> eval altern ~env
    | _ -> raise (Error "If got non bool"))
  | _ ->
   (* special case to not error on unbound identifiers for define *)
    (let (args : Expr.t list) =
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
     | [] -> raise (Error "Somethin wrong")
     | hd :: args ->
       match hd with
       | V_builtin func ->
         (match Map.find_exn env func with
          | Variable _ -> raise (Error "awef")
          | Recursive _ -> raise (Error "awefawef")
          | Builtin func -> func args env)
       | V_fun (params, expr, env) ->
         let add_arg_to_env env param arg =
           Map.set env ~key:param ~data:(Variable arg)
         in
         let new_env = List.fold2_exn params args ~init:env ~f:add_arg_to_env
         in
         eval expr ~env:new_env
       | _ -> raise (Error "Function called on non-function"))
;;




let eval_expr_with_env (expr : Expr.t) (env : environment) : t * environment =
  eval expr ~env
;;

let eval_expr (expr : Expr.t) : t * environment =
  let starter_env = get_starter_env () in
  eval_expr_with_env expr starter_env
;;



(* BEGIN tests ----------------------------------------------------- *)


