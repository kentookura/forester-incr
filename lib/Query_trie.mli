open Forester_core

type key = addr Query.t
type 'a t
type 'a tf = 'a option -> 'a option

val lift_tf : ('a t -> 'b) -> 'a t option -> 'b option
val empty : 'a t
val lookup : key -> 'a t -> 'a option
val update : key -> 'a tf -> 'a t -> 'a t
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val cardinal : 'a t -> int
val elems : 'a t -> 'a list
val union_with : 'a. ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
