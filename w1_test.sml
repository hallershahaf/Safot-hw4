use "w1.sml";

exception AssertError;
fun assert b = if b then () else raise AssertError;

assert (CounterImpl.value() = 0);
assert (CounterImpl.inc() = ());
assert (CounterImpl.value() = 1);
assert (CounterImpl.dec() = ());
assert (CounterImpl.dec() = ());
assert (CounterImpl.value() = ~1);
assert (CounterImpl.reset() = ());
assert (CounterImpl.value() = 0);