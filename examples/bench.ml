open Core.Std
open Core_bench.Std

type 'a bt_tree = Tip | Root of 'a bt_tree * 'a * 'a bt_tree

let bt_empty = Tip
let rec bt_add tree a =
  match tree with
  | Tip -> Root (Tip, a, Tip)
  | Root (l, e, r) ->
    if a = e then tree
    else if a < e then
      Root (bt_add l a, e, r)
    else
      Root (l, e, bt_add r a)
let rec bt_lookup tree a =
  match tree with
  | Tip -> None
  | Root (l, e, r) ->
    if a = e then Some e
    else if a < e then
      bt_lookup l a
    else
      bt_lookup r a
let rec bt_of_list l =
  List.fold_left ~f:bt_add ~init:bt_empty l

let list =
  let l =
    let lines = ref [] in
    (try while true do
         lines := input_line stdin :: !lines
       done ; !lines
     with
     End_of_file -> !lines) in
  List.map ~f:int_of_string l

let to_query =
  let rec make = function
    | 0 -> []
    | n -> Random.int 999 :: make (n - 1) in
  make 1000

let query_some q l =
  List.fold_left ~init:l ~f:q to_query

let tree_forced =
  let tq =
    let rec make = function
      | 0 -> []
      | n -> Random.int 999 :: make (n - 1) in
    make 10000 in
  List.fold_left ~init:(Ltree.tree_of_list list) ~f:(fun tree x -> let _, tree = Ltree.lookup tree x in tree) tq

let () =
  Command.run (Bench.make_command [
      Bench.Test.create ~name:"vanila build"
        (fun () -> bt_of_list list);
      Bench.Test.create ~name:"ltree build"
        (fun () -> Ltree.tree_of_list list);
      Bench.Test.create ~name:"vanila query"
        (fun () -> query_some (fun tree x -> ignore (bt_lookup tree x); tree) @@ bt_of_list list);
      Bench.Test.create ~name:"ltree query"
        (fun () -> query_some (fun tree x -> let _, tree = Ltree.lookup tree x in tree) @@ Ltree.tree_of_list list);
      Bench.Test.create ~name:"ltree forced query"
        (fun () -> query_some (fun tree x -> let _, tree = Ltree.lookup tree x in tree) tree_forced);
    ])
