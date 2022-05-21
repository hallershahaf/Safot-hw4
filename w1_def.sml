signature Counter = sig
  val inc: unit -> unit
  val dec: unit -> unit
  val reset: unit -> unit
  val value: unit -> int
end;