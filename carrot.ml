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
  | 0 | 1 -> "" (* throw error *)
  | 2 -> argv.(1)
  | _ -> "" (* throw error *)
;;

(* let file_contents = readfile program_file;;

   let () = run_program file_contents;;*)

(* BEGIN TESTS ---------------------------------------------- *)



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
(print (cons 3 (empty none)))
";
    [%expect{|
    (Cons(V_num 3)Empty)
  |}]

  
