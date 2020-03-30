(** {1 Mutable dynamic array} *)

type ('a, -'p) t

(** The 'p type parameter is a phantom type parameter that represents the vector's mutability.
It is [[`R | `W]] for read-write vectors, [[`R]] for read-only vectors, or [[`W]] for write-only vectors. *)

val make : ?growth_rate:float -> ?capacity:int -> unit -> ('a, [`R | `W]) t
(** Constructs a vector with the specified growth rate and capacity. *)

val copy : ('a, [> `R]) t -> ('a, [`R | `W]) t
(** Creates a copy of the specified vector. *)

val as_read_only : ('a, [> `R]) t -> ('a, [`R]) t
(** Reinterprets the specified vector as a read-only vector. *)

val as_write_only : ('a, [> `R]) t -> ('a, [`W]) t
(** Reinterprets the specified vector as a write-only vector. *)

val length : ('a, [> ]) t -> int
(** Returns the length of the specified vector. *)

val capacity : ('a, [> ]) t -> int
(** Returns the capacity of the specified vector. *)

val growth_rate : ('a, [> `R]) t -> float
(** Returns the growth rate of the specified vector. *)

val set_growth_rate : float -> ('a, [`R | `W]) t -> unit
(** Sets the growth rate of the specified vector to the specified value. *)

val unsafe_get : ('a, [> `R]) t -> int -> 'a
(** Gets the value in the vector at the specified index.
    @raise Invalid_argument if the index is out of bounds. *)

val unsafe_set : ('a, [> `W]) t -> int -> 'a -> unit
(** Sets the value in the vector at the specified index to the specified value.
    @raise Invalid_argument if the index is out of bounds. *)

val get : ('a, [> `R]) t -> int -> 'a option
(** Gets the value in the vector at the specified index. Returns None if the index is out of range. *)

val set : ('a, [> `W]) t -> int -> 'a -> bool
(** Sets the value in the vector at the specified index to the specified value. Returns false if the index is out of range. *)

val ensure_capacity : int -> ('a, [> `W]) t -> unit
(** Increases the vector's capacity to be at least as large as the specified value. *)

val reserve : int -> ('a, [> `W]) t -> unit
(** Increases the vector's capacity by the specified value. *)

val shrink_to_fit : ('a, [> `R | `W]) t -> unit
(** Shrinks the vector's internal buffer to only be as large as the vector's length. *)

val push : 'a -> ('a, [> `W]) t -> unit
(** Pushes the specified item onto the end of the vector. *)

val pop : ('a, [> `R | `W]) t -> 'a option
(** Pops off the item from the end of the vector. *)

val map : ('a -> 'b) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
(** Maps the specified function over the vector, returning a new vector. *)

val mapi : (int -> 'a -> 'b) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
(** Like {!map}, but the function also takes the item's index as a parameter. *)

val map_in_place : ('a -> 'a) -> ('a, [`R | `W]) t -> unit
(** Like {!map}, but the transformation is done in-place, so no new vector gets created. *)

val return : 'a -> ('a, [`R | `W]) t
(** Returns a singleton vector containing the specified item. *)

val map2 : ('a -> 'b -> 'c) -> ('a, [> `R]) t -> ('b, [> `R]) t -> ('c, [`R | `W]) t
(** Maps the specified function over all combinations of tuples from the 2 specified vectors, returning a new vector. *)

val apply : ('a -> 'b, [> `R]) t -> ('a, [> `R]) t -> ('b, [`R | `W]) t
(** Applies every function from the first vector to every value from the second vector, returning a new vector. *)

val flatten : (('a, [> `R]) t, [> `R]) t -> ('a, [> `R]) t
(** Flattens nested vectors into a single, flat vector. *)

val flat_map : ('a -> ('b, [> `R]) t) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
(** Like {!map}, but flattened *)

val iter : ('a -> unit) -> ('a, [> `R]) t -> unit
(** Applie the specified function to each item in the vector. *)

val iteri : (int -> 'a -> unit) -> ('a, [> `R]) t -> unit
(** Like {!iter}, but the function also takes the item's index as a parameter. *)

val filter : ('a -> bool) -> ('a, [> `R]) t -> ('a, [`R | `W]) t
(** Returns a new vector containing only the items from the first vector that satisfy the specified predicate. *)

val of_list : 'a list -> ('a, [`R | `W]) t
(** Constructs a vector from the specified list. *)

val to_list : ('a, [> `R]) t -> 'a list
(** Constructs a list from the specified vector. *)

val of_array : 'a array -> ('a, [`R | `W]) t
(** Constructs a vector from the specified array. *)

val to_array : ('a, [> `R]) t -> 'a array
(** Constructs an array from the specified vector. *)

val of_array_steal : 'a array -> ('a, [`R | `W]) t
(** Constructs a new vector using the specified array as a buffer. If the array is modified afterwards, the vector is also modified. *)

val steal : ('a, [> `R | `W]) t -> 'a array
(** Returns the vector's internal buffer, and resets the vector to an empty state. *)

val rev : ('a, [> `R]) t -> ('a, [`R | `W]) t
(** Returns a new vector that contains all the items in the specified vector, but in reverse order. *)

val rev_in_place : ('a, [`R | `W]) t -> unit
(** Reverses the vector in-place, without allocating a new vector. *)

val append : ('a, [`R | `W]) t -> ('a, [> `R]) t -> unit
(** Appends the second vector to the first vector. *)

val any : ('a -> bool) -> ('a, [> `R]) t -> bool
(** Returns [true] if any item in the vector satisfies the specified predicate. *)

val all : ('a -> bool) -> ('a, [> `R]) t -> bool
(** Returns [true] if all items in the vector satisfies the specified predicate. *)

val mem : 'a -> ('a, [> `R]) t -> bool
(** Returns [true] if the specified item exists in the vector. Uses structural equality. *)

val memq : 'a -> ('a, [> `R]) t -> bool
(** Returns [true] if the specified item exists in the vector. Uses physical equality. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> ('a, [> `R]) t -> 'b
(** Folds the specified function and default value over the array, from left to right. *)

val fold_right : ('a -> 'b -> 'b) -> 'b -> ('a, [> `R]) t -> 'b
(** Folds the specified function and default value over the array, from right to left. *)

val zip : ('a, [> `R]) t -> ('b, [> `R]) t -> ('a * 'b, [`R | `W]) t
(** Zips the two vectors together. *)

val zip_with : ('a -> 'b -> 'c) -> ('a, [> `R]) t -> ('b, [> `R]) t -> ('c, [`R | `W]) t
(** Zips the two vectors together, using the specified function to combine values. *)

val sort : ('a, [`R | `W]) t -> unit
(** Sorts the specified vector. *)

val sort_by : ('a -> 'a -> int) -> ('a, [`R | `W]) t -> unit
(** Sorts the specified vector using the specified comparison function. *)

val iota : int -> int -> (int, [`R | `W]) t
(** Constructs a vector containing all numbers in the specified range. *)

module Infix : sig
  val (.![]) : ('a, [> `R]) t -> int -> 'a
  (** Infix version of {!unsafe_get}. *)

  val (.![]<-) : ('a, [> `W]) t -> int -> 'a -> unit
  (** Infix version of {!unsafe_set}. *)

  val (.?[]) : ('a, [> `R]) t -> int -> 'a option
  (** Infix version of {!get}.*)

  val (.?[]<-) : ('a, [> `W]) t -> int -> 'a -> bool
  (** Infix version of {!set}.*)

  val (=|<) : ('a -> 'b) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
  (** Infix version of {!map}.*)

  val (>|=) : ('a, [> `R]) t -> ('a -> 'b) -> ('b, [`R | `W]) t
  (** Infix version of {!map}, with the arguments flipped. *)
  
  val (<$>) : ('a -> 'b) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
  (** Infix version of {!map}.*)

  val (<*>) : ('a -> 'b, [> `R]) t -> ('a, [> `R]) t -> ('b, [`R | `W]) t
  (** Infix version of {!apply}.*)

  val (=<<) : ('a -> ('b, [> `R]) t) -> ('a, [> `R]) t -> ('b, [`R | `W]) t
  (** Infix version of {!flat_map}.*)

  val (>>=) : ('a, [> `R]) t -> ('a -> ('b, [> `R]) t) -> ('b, [`R | `W]) t
  (** Infix version of {!flat_map}, with the arguments flipped.*)

  val (--) : int -> int -> (int, [`R | `W]) t
  (** Infix version of {!iota}.*)
end
(** Contains infix versions of some vector operations. *)