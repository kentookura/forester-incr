type key
type 'a t
type 'a tf = 'a option -> 'a option

val empty : 'a t
val lookup : key -> 'a t -> 'a option
val update : key -> 'a tf -> 'a t -> 'a t
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val cardinal : 'a t -> int
val elems : 'a t -> 'a list
val union_with : 'a. ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
