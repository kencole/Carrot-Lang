open Core;;
    
let readfile = In_channel.read_all;;

let run_program s =
  let pass_along_env env statement : Value.environment option * Value.t =
    let (result, env) = 
      match env with
      | None -> Value.eval_expr statement
      | Some env -> Value.eval_expr_with_env statement env
    in
    (Some env, result)
  in
  (ignore  
     (Expr.parse_file_body s
      |> List.folding_map ~f:pass_along_env ~init:None : Value.t list))
;;

let program_file =
  let argv = Sys.get_argv () in
  match Array.length argv with
  | 0 | 1 -> Error.raise (Error.of_string "Expected input file name")
  | 2 -> argv.(1)
  | _ -> Error.raise (Error.of_string "Too many input files")
;;

(* let file_contents = readfile program_file;;

   let () = run_program file_contents;; *)

(* BEGIN TESTS ---------------------------------------------- *)

(* GRAMMER has
print
define
deffun
deffun*
cons
empty
first
rest
is-empty
if
lambda
=
and
or

none
true
false
empty
*)



let%expect_test _ =
  run_program "(print (+ 1 1) (+ 2 3))";
    [%expect{|
    2 5
  |}]

let%expect_test _ =
  run_program "(print (+ (* 2 3) 1))";
    [%expect{|
    7
  |}]

let%expect_test _ =
  run_program "(print (+ (* 2 3) 1)) (print (+ 1 2))";
    [%expect{|
    7
    3
  |}]

let%expect_test _ =
  run_program "(define x 2) (print (+ x 2))";
    [%expect{|
    4
  |}]

let%expect_test _ =
  run_program "
(print (/ 1 2))
(print (+ 1 1))
(print (+ (+ (+ (+ 1)))))
(print (* 1 2 3 4 5))

";
    [%expect{|
    0
    2
    1
    120
  |}]

let%expect_test _ =
  run_program "
(define a 1)
(print a)
";
    [%expect{|
    1
  |}]

let%expect_test _ =
  run_program "
(define a \"a\")
(print a)
";
    [%expect{|
    a
  |}]

let%expect_test _ =
  run_program "
(define a \"a 2\")
(print a)
";
    [%expect{|
    a 2
  |}]

let%expect_test _ =
  run_program "
(define a \"a 2\")
(print a)
";
    [%expect{|
    a 2
  |}]


let%expect_test _ =
  run_program "
(define a 3)
(print a)
(define a 4)
(print a)
(print (+ a a))
";
    [%expect{|
    3
    4
    8
  |}]

let%expect_test _ =
  run_program "
(define a (print 2))
(print a)
";
    [%expect{|
    2
    None
  |}]

let%expect_test _ =
  run_program "
(define prnt 2)
(print 3)
";
    [%expect{|
    3
  |}]

let%expect_test _ =
  run_program "
(print (cons 3 empty))
";
    [%expect{|
    [3]
  |}]
  
let%expect_test _ =
  run_program "
(deffun f x (+ x x))
(print (f 2))
";
    [%expect{|
    4
  |}]


let%expect_test _ =
  run_program "
(deffun f x (cons 3 x))
(print (f empty))
(print (empty? empty))
(print (empty? (cons 3 empty)))
(print (first (cons 3 empty)))
(print (rest (cons \"a\" empty)))
(deffun g l (if (empty? l) 1 2))
(print (g empty))
(print (g (cons 1 empty)))
";
    [%expect{|
    [3]
    true
    false
    3
    []
    1
    2
  |}]

let%expect_test _ =
  run_program "
(deffun* f n 
  (if (= n 0)
      0
      (f (+ n -1)))))
(print (f 10))
";
    [%expect{|
    0
  |}]

let%expect_test _ =
  run_program "
(deffun* fib x
 (if (or (= x 0) (= x 1))
     1
     (+ (fib (- x 1))
	(fib (- x 2))))))
(print (fib 10))
";
    [%expect{|
    89
  |}]


let%expect_test _ =
  run_program "
(deffun add a b (+ a b))
(print (add 11 10))
";
    [%expect{|
    21
  |}]
    
let%expect_test _ =
  run_program "
(deffun* map f l
 (if (empty? l)
     l
     (cons (f (first l))
           (map f (rest l))))) 
(deffun double x (+ x x))
(define list (cons 1 (cons 2 (cons 3 empty))))
(print (map double list))
";
    [%expect{|
    [6, 4, 2]
  |}]

let%expect_test _ =
  run_program "
(deffun* fold f l acc
  (if (empty? l)
      acc
      (fold f (rest l) (f acc (first l)))))

(deffun add2 a b (+ a b))

(deffun* range x
  (if (= x -1)
      empty
      (cons x (range (- x 1)))))

(print (fold add2 (range 10000) 0))

(print (range 10))
(print empty)
";
    [%expect{|
    50005000
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    []
  |}]



