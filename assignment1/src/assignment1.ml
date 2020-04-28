let rec cond_dup l f =
  match l with
  | [] -> []
  | h :: t -> if f h then (h :: [h]) @ (cond_dup t f)
    else h :: (cond_dup t f);;

(* assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5]) *)

let rec n_times (f, n, v) =
  if n <= 0
  then v
  else n_times(f, n - 1, f v);;

(* assert (n_times((fun x-> x+1), 50, 0) = 50) *)

exception IncorrectRange

let rec range num1 num2 =
  if num2 < num1 then raise(IncorrectRange)
  else if num1 = num2 then [num2]
  else num1 :: range (num1 + 1) num2;;
(* assert (range 2 5 = [2;3;4;5]) *)

let rec zipwith f l1 l2 =
  match (l1, l2) with
    | ([], _) -> []
    | (_, []) -> []
    | (h1 :: t1, h2 :: t2) -> f h1 h2 :: zipwith f t1 t2;;

(* assert (zipwith (+) [1;2;3] [4;5] = [5;7]) *)

let buckets p l =
  (*let rec buck curr acc = 
    match l with
    | [] -> []
    | [x] -> (x :: curr) :: acc
    | a :: (b :: _ as t) ->
      if a = b then buck (a :: curr) :: acc t
      else buck [] ((a :: curr) :: acc) t in
    List.rev (buck [] l);;*)
  let rec buck f hac accu=(* helper method to put things into buckets so to speak*)
    match accu with
    | [] -> [[hac]]
    | h :: t -> 
      match h with
      | [] -> []
      | hd :: _ ->
        if f hd hac = true(*comments because this is almost laughably confusing*)
          then (h @ [hac]) :: t 
        else h :: (buck f hac t) in
    let rec it f lst iaccu =(*iterates through list*)
      match lst with
      | [] -> iaccu(*if empty, return iaccu*)
      | hit :: tit -> it f tit (buck f hit iaccu) in(*else compare head with parsing method*)
      it p l [](*this essentially jumpstarts the iteration method*)
      ;;

   

(* assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]) *)
(* assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]) *)
(* assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]]) *)

let rec remove_stutter l =
  match l with
  | h :: (t :: _ as x) -> if h = t then remove_stutter x
    else h :: remove_stutter x
  | y -> y ;;

(* assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]) *)

let rec flatten l =
  match l with
  | [] -> []
  | h :: t -> h @ (flatten t);;

(* assert (flatten ([[1;2];[3;4]]) = [1;2;3;4]) *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec fold_inorder f acc t =
  match t with
  |Node(l, n, r) -> let ar = fold_inorder f acc r in
    let an = n :: ar in
      fold_inorder f an l
  |Leaf -> acc;;
  

(* assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]) *)

let fib_tailrec n =
  let rec fib i cur prev =
    if i = n then cur
    else fib (i + 1) (prev) (cur + prev) in
  fib 0 0 1

(* assert (fib_tailrec 50 = 12586269025) *)