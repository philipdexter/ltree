open Ltree

let main () =
  let t = List.fold_left add empty [1; 2; 3; 4; 5] in
  let (a, t) = lookup t 2 in
  let (a, t) = lookup t 5 in
  print_endline (string_of_tree t (string_of_int))

let () = main ()
