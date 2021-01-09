open Core;;

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

let find_matching_close_quote s i =
  let break = ref false in
  let i = ref (i + 1) in
  let length = String.length s in
  while not !break && !i < length do
    (match String.get s !i with
     | '"' -> break := true
     | _ -> ());
    i := !i + 1
  done;
  match !break with
  | true -> Some (!i - 1)
  | false -> None
;;

let find_matching_close s i =
  let open_parens = ref 1 in
  let i = ref (i + 1) in
  let length = String.length s in
  while !open_parens > 0 && !i < length do
    (match String.get s !i with
      | '(' -> open_parens := 1 + !open_parens
      | ')' -> open_parens := (-1) + !open_parens
      | _ -> ());
    i := !i + 1
  done;
  match !open_parens with
  | 0 -> Some (!i - 1)
  | _ -> None
;;

let get_toplevel_statements file_body =
  let statements = ref [] in
  let i = ref 0 in
  let length = String.length file_body in
  while !i < length do
    match String.get file_body !i with
    | '(' ->
      (match find_matching_close file_body !i with
       | None -> i := length (* break *)
       | Some close ->
         let new_statement =
           String.sub ~pos:!i ~len:(close - !i + 1) file_body
         in
         statements := new_statement :: !statements;
         i := close)
    | _ -> i := !i + 1
  done;
  List.rev !statements
;;

let get_statement_tokens statement =
  let is_whitespace c =
    match c with
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false
  in
  let length = String.length statement in
  let find_token_end i =
    let rec helper i = 
      if i >= length then
        length
      else
        let c = String.get statement i in
        match (is_whitespace c) || Char.(c = ')') with
        | true -> i
        | false -> helper (i + 1)
    in match String.get statement i with
    | '"' ->
      (match find_matching_close_quote statement i with
       | Some v -> v + 1
       | None -> -1) (* throw some error here ? *)      
    | '(' ->
      (match find_matching_close statement i with
       | Some v -> v + 1
       | None -> -1) (* throw some error here ? *)
    | _ -> helper i
  in
  let tokens = ref [] in
  let start =
    if length = 0 then
      0
    else
      match String.get statement 0 with
      | '(' -> 1
      | _ -> 0
  in
  let i = ref start in
  while !i < length do
    let c = String.get statement !i in
    match is_whitespace c with
    | true -> i := !i + 1
    | false ->
      let token_end = find_token_end !i in
      let new_token =
        String.sub ~pos:!i ~len:(token_end - !i) statement
      in
      tokens := new_token :: !tokens;
      i := token_end + 1
  done;
  List.rev !tokens
;;

let parse_operator = function
  | "+" -> Some O_plus
  | "-" -> Some O_minus
  | "*" -> Some O_times
  | "/" -> Some O_divide
  | _ -> None 
;;

let parse_literal lit =
  try E_num (Int.of_string lit)
  with Failure _ ->
    let length = String.length lit in
    match length with
    | 0 | 1 -> E_symb lit
    | _ ->
      (let first = String.get lit 0 in
       let last = String.get lit (length - 1) in
       match first, last with
       | ('"', '"') -> E_str (String.sub ~pos:1 ~len:(length - 2) lit)
       | _ -> E_symb lit)
;;
    
let rec parse_statement statement =
  let tokens = get_statement_tokens statement in
  match tokens with
  | []-> parse_literal statement
  | [token] -> parse_literal token
  | f::r -> (match parse_operator f with
      | Some op -> E_op (op, List.map r ~f:parse_statement)
      | None -> E_app (List.map tokens ~f:parse_statement))
;;

let parse_file_body file_body =
  List.map ~f:parse_statement (get_toplevel_statements file_body)


(* Begin tests ------------------------------------------------- *)

(*

let print_int i =
  Int.to_string i
  |> Out_channel.output_string stdout
;;

(*
let print_int_newline i =
  print_int i;
  print_string "\n"
;; *)

let print_int_opt i =
  Option.value_exn i
|> print_int
;;

let print_newline s =
  print_string s;
  print_string "\n"
;;


(* get_toplevel_statements tests ------------------------------- *)


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


(* find_matching_close tests -------------------------------------- *)

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

(* get_toplevel_statements tests ----------------------------------- *)

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

(* get_statement_tokens tests --------------------------------------- *)

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
  List.iter ~f:print_newline (get_statement_tokens "(a \"b d\" c)");
    [%expect{|
    a
    "b d"
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


(* parse_statement tests -------------------------------------------- *)



let print_parsed_statements s =
  Sexp.to_string [%sexp (parse_statement s : t)]
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
