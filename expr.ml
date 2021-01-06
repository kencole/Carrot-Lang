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

(*
let get_statements file_body =
  let find_parens i accum c =
    if Char.(c = '(') then
      (i, 1) :: accum
    else if Char.(c = ')') then
      (i, -1) :: accum
    else accum
  in    
  let parens =
    String.foldi ~init:[] ~f:find_parens file_body
    |> List.rev
  in
  let cumulative_sum accum (i, open_or_close) =
    match List.hd accum with
    | None -> [(i, open_or_close)]
    | Some (_, curr_sum) -> (i, open_or_close + curr_sum) :: accum
  in
  let parens =
    (-1, 0) :: (List.fold parens ~f:cumulative_sum ~init:[]
                |> List.rev)
  in
  let get_statements_beginning_and_end accum prior current =
    let (_prior_i, prior_open) = prior in
    let (curr_i, curr_open) = current in
    match (prior_open, curr_open) with
    | (0, 1) -> (curr_i, 1) :: accum
    | (1, 0) -> (curr_i, -1) :: accum
    | _ -> accum
  in
  let priors =
    let all_but_last_parens =
      List.take parens ((List.length parens) - 1) in
    (0, 0) :: all_but_last_parens
  in
  let current = parens in
  let statement_bounds =
    List.fold2_exn priors current
      ~f:get_statements_beginning_and_end ~init:[]
    |> List.rev
  in
  let rec statement_yanker l accum =
    match l with
    | (s_open, _) :: (close, _) :: r ->
      statement_yanker r 
      ((String.sub file_body ~pos:s_open ~len:(close - s_open + 1)) :: accum)
    | [] -> accum
    | _ -> []
  in statement_yanker statement_bounds []
   |> List.rev
;;
*)

let parse_operator = function
  | "+" -> Some O_plus
  | "-" -> Some O_minus
  | "*" -> Some O_times
  | "/" -> Some O_divide
  | _ -> None (* throw error here *)
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
