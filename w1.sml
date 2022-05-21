use "w1_def.sml";

structure CounterImpl :> Counter = struct 
    val counter = ref 0;
    fun inc() = counter := !counter + 1;
    fun dec() = counter := !counter - 1;
    fun reset() = counter := 0;
    fun value() = !counter;
end;