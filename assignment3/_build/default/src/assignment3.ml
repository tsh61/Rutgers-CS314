(******************** Problem 1 ********************)

type 'a element = {
  content : 'a;
  mutable next : 'a element option;
  mutable prev : 'a element option
}

let create () = ref None

let is_empty t = !t = None

let insert_first l c =
  let n = {content = c; next = !l; prev = None} in
  let _ = match !l with
    | Some o -> (o.prev <- Some n)
    | None -> () in
  let _ = (l := Some n) in
  n

let insert_after n c =
  let n' = {content = c; next = n.next; prev = Some n} in
  let _ =  match n.next with
    | Some o -> (o.prev <- (Some n'))
    | None -> () in
  let _ = (n.next <- (Some n')) in
  n'

let remove t elt =
  let prev, next = elt.prev, elt.next in
  let _ = match prev with
    | Some prev -> (prev.next <- next)
    | None -> t := next in
  let _ =  match next with
    | Some next -> (next.prev <- prev)
    | None -> () in
  let _ = (elt.prev <- None) in
  let _ = (elt.next <- None) in
  ()      (* return void *)

let iter t f =
  let rec loop node =
    match node with
      | None -> ()
      | Some el ->
        let next = el.next in
        let _ = f el in
        loop (next)
  in
  loop !t

let dll_of_list l =
  let dlist = create() in 
  let rec loop l1 pre =
    match l1 with
    | [] -> dlist (* if empty list, return empty list *)
    | h :: t -> loop t (insert_after pre h)
    in
    match l with
    | [] -> dlist
    | h :: t -> loop t (insert_first dlist h)

let list_of_dll l =
  let linked = ref [] in
  let _ = iter l (fun n ->
    let _ = (linked := !linked@[n.content]) in
    match n.next with
    | None -> []
    | Some _ -> !linked) in
    !linked


let length l =
  let counter = ref 0 in
  let _ = iter l (fun n ->
    let _ = (counter := !counter + 1) in
    match n.next with
    | None -> !counter
    | Some _ -> !counter) in
    !counter

let duplicate l =
  iter l (fun n ->
    insert_after n n.content)

let reverse l =
  iter l (fun n ->
    let nx = n.content in
      let _ = remove l n in
        insert_first l nx)

(******************** Problem 2 ********************)

module type Serializable = sig
  type t
  type content

  val string_of_t : t -> string

  val fold : ('a -> content -> 'a) -> 'a -> t -> 'a
end

module SerializableList (C : Serializable) = struct
  type t = C.t list
  type content = C.t

  let string_of_t l =
    let rec loop acc l = match l with
      | [] -> acc
      | [x] -> acc ^ (C.string_of_t x)
      | x::xs -> loop (acc ^ (C.string_of_t x) ^ ";") xs
    in
    "[" ^ (loop "" l) ^ "]"

  let fold f accum l =
    List.fold_left (C.fold f) accum l
end

module SerializableArray (C : Serializable) = struct
  type t = C.t array
  type content = C.t

  let string_of_t l =
    let ln = Array.to_list l in
    let rec loop acc ln = match ln with
      | [] -> acc
      | [x] -> acc ^ (C.string_of_t x)
      | x::xs -> loop (acc ^ (C.string_of_t x) ^ ";") xs
    in
    "[|" ^ (loop "" ln) ^ "|]"

  let fold f accum l =
    Array.fold_left (C.fold f) accum l
end

module SerializableIntArray = SerializableArray (struct
  type t = int
  type content = int

  let string_of_t x = string_of_int x

  let fold f i res = f i res
end)

module SerializableIntList = SerializableList (struct
  type t = int
  type content = int

  let string_of_t x = string_of_int x

  let fold f i res = f i res
end)

module SerializableIntArrayArray = SerializableArray(SerializableIntArray)

module SerializableIntListArray = SerializableArray(SerializableIntList)

module SerializableIntArrayList = SerializableList(SerializableIntArray)
