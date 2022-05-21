use "w2_def.sml";

structure ListIterable : Iterable = struct
  exception IllegalOperation
  datatype ListIter = Iter of int list * int;
  fun getList(Iter(l, i) : ListIter) = l;
  fun getIndex(Iter(l, i) : ListIter) = i;
  fun isNone(NONE : 'a option) = true
    | isNone(SOME(a) : 'a option) = false;
  type container = int list;
  type entry = int;
  type iterator = ListIter option;
  fun makeIterator([]) = NONE
    | makeIterator(l : container) = SOME(Iter(l, 0));
  val emptyIterator = NONE;
  fun rEnd(iter : iterator) = isNone(iter) orelse (getIndex(valOf(iter)) = length(getList(valOf(iter))));
  fun lEnd(iter : iterator) = isNone(iter) orelse (getIndex(valOf(iter)) = ~1);
  fun next(iter : iterator) = if rEnd(iter) then 
                                  raise IllegalOperation
                              else
                                  SOME(Iter(getList(valOf(iter)),(getIndex(valOf(iter)) + 1)));
  fun prev(iter : iterator) = if lEnd(iter) then 
                                  raise IllegalOperation
                              else
                                  SOME(Iter(getList(valOf(iter)),(getIndex(valOf(iter)) - 1)));
  fun value(iter : iterator) = if rEnd(iter) orelse lEnd(iter) then
                                  raise IllegalOperation
                               else
                                  List.nth(getList(valOf(iter)), getIndex(valOf(iter)));
end;

structure SeqIterable : Iterable = struct
  exception IllegalOperation
  datatype twoSidedSeq = END | Node of twoSidedSeq option * string seq;
  fun first (Node(x, _)) = x;
  fun last (Node(_, x)) = x;
  fun head (Cons (x, _)) = x;
  fun tail (Cons (_, x)) = x();
  fun isNil(Nil) = true
    | isNil(Cons(_, _)) = false;
  fun isNone(NONE : 'a option) = true
    | isNone(SOME(a) : 'a option) = false;
  fun isEnd(END : twoSidedSeq) = true
    | isEnd(Node(_, _) : twoSidedSeq) = false;
  type container = string seq;
  type entry = string;
  type iterator = twoSidedSeq option;
  fun makeIterator(Nil) = NONE
    | makeIterator(cont : container) = SOME(Node(SOME(END), cont));
  val emptyIterator = NONE;
  fun rEnd(NONE : iterator) = true
    | rEnd(iter : iterator) = isNil(last (valOf(iter)));
  fun lEnd(NONE : iterator) = true
    | lEnd(iter : iterator) = isNone(first (valOf(iter)));
  fun next(iter : iterator) = if rEnd(iter) then
                                  raise IllegalOperation
                              else
                                  if isNone(first (valOf(iter))) then
                                      SOME(Node(SOME(END), last (valOf(iter))))
                                  else
                                      SOME(Node(iter, tail (last (valOf(iter)))));
  fun prev(iter : iterator) = if lEnd(iter) then
                                    raise IllegalOperation
                                else
                                    if isEnd(valOf(first (valOf(iter)))) then
                                        SOME(Node(NONE, last (valOf(iter))))
                                    else
                                        SOME(Node(first (valOf(first (valOf(iter)))), last (valOf(first (valOf(iter))))));
  fun value(iter : iterator) = if rEnd(iter) orelse lEnd(iter) then 
                                    raise IllegalOperation
                                else
                                    head (last (valOf(iter)));
end;

functor MakeModIterator (IterableModule: Iterable) : Iterator = struct
  exception IllegalOperation;
  type container = IterableModule.container;
  type entry = IterableModule.entry;
  val itrRef = ref IterableModule.emptyIterator;
  fun init(cont : container) = itrRef := IterableModule.makeIterator(cont);
  fun value() = (!itrRef; IterableModule.value(!itrRef));
  fun next() = itrRef := IterableModule.next(!itrRef);
  fun prev() = itrRef := IterableModule.prev(!itrRef);
  fun hasNext() = not (IterableModule.rEnd(!itrRef));
  fun hasPrev() = not (IterableModule.lEnd(!itrRef));
end;

fun sumAll() = if D_Iterator.hasNext() then
                  (D_Iterator.value() handle IllegalOperation => 0) + (D_Iterator.next(); sumAll())
                else
                  D_Iterator.value() handle IllegalOperation => 0;
