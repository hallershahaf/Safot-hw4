datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

signature Iterable = sig
  exception IllegalOperation
  type container
  type entry
  type iterator
  val makeIterator: container -> iterator
  val emptyIterator: iterator
  val rEnd: iterator -> bool
  val lEnd: iterator -> bool
  val next: iterator -> iterator
  val prev: iterator -> iterator
  val value: iterator -> entry
end;

signature Iterator = sig
  exception IllegalOperation
  type container
  type entry
  val init: container -> unit
  val value: unit -> entry
  val next: unit -> unit
  val prev: unit -> unit
  val hasNext: unit -> bool
  val hasPrev: unit -> bool
end;

(* For example only! We will check your code with diffrent iterators *)
structure D_Iterator : Iterator = struct
  val i = ref 0
  exception IllegalOperation;
  type container = unit
  type entry = int
  fun init () = ()
  fun value () = if !i = 10 orelse !i = ~1 then raise IllegalOperation else !i
  fun next () = if !i = 10 then raise IllegalOperation else i := !i + 1
  fun prev () = if !i = ~1 then raise IllegalOperation else i := !i - 1
  fun hasNext () = !i < 10
  fun hasPrev () = !i > ~1
end;