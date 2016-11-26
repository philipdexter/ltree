type 'a tree

val empty : 'a tree

val add : 'a tree -> 'a -> 'a tree

val lookup : 'a tree -> 'a -> 'a option * 'a tree

val tree_of_list : 'a list -> 'a tree

val string_of_tree : 'a tree -> ('a -> string) -> string

val num_delayed : 'a tree -> int
