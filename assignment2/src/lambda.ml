open Syntax
let parse_string = Lambda_parse.parse_string
let string_of_expr = string_of_expr

let r = ref 0

let fresh s =
  let v = !r in
  r := !r + 1;
  s ^ (string_of_int v)

(* Your implementation begins from here *)

let mem e l =
  (* YOUR CODE HERE *)
  match l with
  |[] -> false
  raise (Failure "Problem 1 not implemented")



let remove e l =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 2 not implemented")



let union l1 l2 =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 3 not implemented")



let add e l =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 4 not implemented")



let rec free_variables e =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 5 not implemented")



let rec substitute expr a b =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 6 not implemented")



let rec reduce_cbv e =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 7 not implemented")



let rec reduce_cbn e =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 8 not implemented")



let rec reduce_normal e =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 9 not implemented")


(* Your implementation done here *)

(* Debug your code by printing out evaluation results *)
let rec eval log depth reduce expr =
  if depth = 0 then failwith "non-termination?"
  else begin
    let expr', reduced = reduce expr in
    if not reduced then expr else begin
      if log then print_endline ("= " ^ (string_of_expr expr'));
      eval log (depth-1) reduce expr'
    end
  end
let eval_cbv = eval true 1000 reduce_cbv
let eval_cbn = eval true 1000 reduce_cbn
let eval_normal = eval true 1000 reduce_normal

(* To debug and observe the evaluation steps of your `reduce_cbv`, `reduce_cbn`
 * or `reduce_normal` implementation, use the following code.
 *
 *let _ = eval_cbv (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_cbn (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_normal (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *)
