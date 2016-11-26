(* TODO: write tests, like total delayed + total realized = total added *)

type 'a delayed = 'a list

type 'a tree =
  | Tip
  | Root of 'a tree * 'a * 'a tree * 'a delayed

let empty = Tip

let add tree n =
  match tree with
  | Tip -> Root (Tip, n, Tip, [])
  | Root (l, e, r, d) -> Root (l, e, r, (n :: d))

(* what if x = n ? *)
let gather l n =
  let rec gather l n (lt,gt) =
    match l with
    | [] -> (lt, gt)
    | x::xs -> if x < n
      then gather xs n (x::lt, gt)
      else gather xs n (lt, x::gt) in
  gather l n ([], [])

let rec lookup tree a =
  match tree with
  | Tip -> (None, tree)
  | Root (l, e, r, d) ->
    if a = e then
      (Some e, tree)
    else
      let (lt, gt) = gather d e in
      if a < e then
        let l' = match lt with
          | [] -> l
          | x::xs -> begin
              match l with
              | Tip -> Root (Tip, x, Tip, xs)
              | Root (l, e, r, d) -> Root (l, e, r, lt@d)
            end in
        let r' = match gt with
          | [] -> r
          | x::xs -> begin
              match r with
              | Tip -> Root (Tip, x, Tip, xs)
              | Root (l, e, r, d) -> Root (l, e, r, gt@d)
            end in
        let (answer, l') = lookup l' a in
        (answer, Root (l', e, r', []))
      else
        let r' = match gt with
          | [] -> r
          | x::xs -> begin
              match r with
              | Tip -> Root (Tip, x, Tip, xs)
              | Root (l, e, r, d) -> Root (l, e, r, gt@d)
            end in
        let l' = match lt with
          | [] -> l
          | x::xs -> begin
              match l with
              | Tip -> Root (Tip, x, Tip, xs)
              | Root (l, e, r, d) -> Root (l, e, r, lt@d)
            end in
        let (answer, r') = lookup r' a in
        (answer, Root (l', e, r', []))

let tree_of_list l =
  List.fold_left add empty l

let rec string_of_tree tree fmt =
  match tree with
  | Tip -> "Tip"
  | Root (l, e, r, d) -> Printf.sprintf "Root (%s) %s (%s) (%s)\n"
                           (string_of_tree l fmt)
                           (fmt e)
                           (string_of_tree r fmt)
                           (String.concat ", " (List.map fmt d))

let rec num_delayed tree =
  match tree with
  | Tip -> 0
  | Root (l, _, r, d) -> List.length d + num_delayed l + num_delayed r
