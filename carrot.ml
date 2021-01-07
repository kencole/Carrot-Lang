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
  Expr.parse_file_body s
  |> List.folding_map ~f:pass_along_env ~init:None
  |> List.iter ~f:Value.print_value
;;

let program_file =
  let argv = Sys.get_argv () in
  match Array.length argv with
  | 0 | 1 -> "" (* throw error *)
  | 2 -> argv.(1)
  | _ -> "" (* throw error *)
;;

let file_contents = readfile program_file;;

let () = run_program file_contents;;

(* BEGIN TESTS ---------------------------------------------- *)

 (*

let%expect_test _ =
  run_program "(+ 1 1)";
    [%expect{|
    2
  |}]

let%expect_test _ =
  run_program "(+ (* 2 3) 1)";
    [%expect{|
    7
  |}]

let%expect_test _ =
  run_program "(+ (* 2 3) 1) (+ 1 2)";
    [%expect{|
    7
    3
  |}]

let%expect_test _ =
  run_program "(define x 2) (+ x 2)";
    [%expect{|
    None
    4
  |}]

let%expect_test _ =
  run_program "
(/ 1 2)
(+ 1 1)
(+ (+ (+ (+ 1))))
(* 1 2 3 4 5)

";
    [%expect{|
    0
    2
    1
    120
  |}]

*)
