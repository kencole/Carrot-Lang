open Base;;
open Core;;

include Expr

let readfile = In_channel.read_all;;

let print_int i =
  Int.to_string i
  |> Out_channel.output_string stdout
;;

let print_int_newline i =
  print_int i;
  print_string "\n"
;;

let print_int_opt i =
  Option.value_exn i
|> print_int
;;

let print_newline s =
  print_string s;
  print_string "\n"
;;

type value =
  | V_num of int
  | V_str of string

let val_to_ints_then_fold f init args =
  let val_to_int = function
    | V_num v -> v
    | _ -> 0 (* error here? *)
  in
  List.map ~f:val_to_int args
  |> List.fold ~init ~f
;;

let rec eval expr =
  match expr with
  | E_num n -> V_num n
  | E_str s -> V_str s
  | E_op (op, args) ->
    (let args = List.map ~f:eval args in
     match op with
     | O_plus -> V_num (val_to_ints_then_fold Int.(+) 0 args)
     | O_minus -> V_num (val_to_ints_then_fold Int.(-) 0 args)
     | O_times -> V_num (val_to_ints_then_fold Int.( * ) 1 args)
     | O_divide -> V_num (val_to_ints_then_fold Int.( / ) 1 args))
  | E_app _args -> V_num (-1)
  | E_symb s -> V_str s
;;

let print_value = function
  | V_num n -> print_int_newline n
  | V_str s -> print_newline s

let run_program s =
  Expr.parse_file_body s
  |> List.map ~f:eval
  |> List.iter ~f:print_value
;;

let t = "
(+ 1 2 (* 2 4 5))
(* 2 3)
(10) 
(150)
"

let () = run_program t

(*

(* get_toplevel_statements tests *)

let print_statements s =
  let s = get_toplevel_statements s in
  let f s =
    print_string s;
    print_string "\n"
  in
  List.iter ~f s

let%expect_test _ =
  print_statements "()";
    [%expect{|
    ()
  |}]

let%expect_test _ =
  print_statements "(a)";
    [%expect{|
    (a)
  |}]

let%expect_test _ =
  print_statements "(a)(b)(c)";
    [%expect{|
    (a)
    (b)
    (c)
  |}]

let%expect_test _ =
  print_statements "(a()()b)(c)";
    [%expect{|
    (a()()b)
    (c)
  |}]

let%expect_test _ =
  print_statements "(a()()b)(c)";
    [%expect{|
    (a()()b)
    (c)
  |}]

let%expect_test _ =
  print_statements
    "(a()()b)\n 
    (define x 5)
    (c)";
    [%expect{|
    (a()()b)
    (define x 5)
    (c)
  |}]

let test1_expr = "(define 1 2 (- 2 3))";;

let%expect_test _ =
  print_statements test1_expr;
    [%expect{|
   (define 1 2 (- 2 3))
  |}]


(* find_matching_close tests *)

let%expect_test _ =
  print_int_opt (find_matching_close "(a()()b)(c)" 0);
    [%expect{|
     7
  |}]

let%expect_test _ =
  print_int_opt (find_matching_close "(a()()b)(c)" 2);
    [%expect{|
     3
  |}]

let%expect_test _ =
  print_int_opt (find_matching_close "(((()(()))))" 1);
    [%expect{|
     10
  |}]

(* get_toplevel_statements tests *)

let%expect_test _ =
  List.iter ~f:print_string (get_toplevel_statements "()");
    [%expect{|
    ()
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_toplevel_statements "()s(a)(ab(d))");
    [%expect{|
    ()
    (a)
    (ab(d))
  |}]

(* get_statement_tokens tests *)

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "a");
    [%expect{|
    a
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "define");
    [%expect{|
    define
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "define a");
    [%expect{|
    define
    a
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "(a)");
    [%expect{|
    a
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "(a b c)");
    [%expect{|
    a
    b  
    c
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "(a (b c))");
    [%expect{|
    a
    (b c)  
  |}]

let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens "(add 1 2 
(b c) 3 4 5)");
    [%expect{|
    add
    1
    2
    (b c)  
    3
    4
    5
  |}]

    
let%expect_test _ =
  List.iter ~f:print_newline (get_statement_tokens test1_expr);
    [%expect{|
    define
    1
    2
    (- 2 3)
  |}]


(* parse_statement tests *)

let print_parsed_statements s =
  Sexp.to_string [%sexp (parse_statement s : expr)]
  |> print_string

let%expect_test _ =
  print_parsed_statements "1";
    [%expect{|
    (E_num 1)
  |}]

let%expect_test _ =
  print_parsed_statements "(- 2 3)";
    [%expect{|
    (E_op O_minus((E_num 2)(E_num 3)))
  |}]

let%expect_test _ =
  print_parsed_statements "(define 2 3)";
    [%expect{|
    (E_app((E_symb define)(E_num 2)(E_num 3)))
  |}]

let%expect_test _ =
  print_parsed_statements "(define x \"10a\")";
    [%expect{|
    (E_app((E_symb define)(E_symb x)(E_str 10a)))
  |}]


    *)
