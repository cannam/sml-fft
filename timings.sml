        
(* Benchmarking *)
        
fun make_input_reals n =
    Array.tabulate (n, fn x => (Real.fromInt (x mod 2)) / 4.0);

fun make_input_imags n =
    Array.tabulate (n, fn x => 0.0);

fun once n f =
    let val real = make_input_reals n
        val imag = make_input_imags n
    in
        Fft.forward_inplace (f, real, imag);
        foldl (fn (i, acc) => acc + Math.sqrt (Array.sub (real, i) *
                                               Array.sub (real, i) +
                                               Array.sub (imag, i) *
                                               Array.sub (imag, i)))
              0.0
              (List.tabulate (n, fn x => x))
    end

fun once_real n f =
    let val input = Array.vector (make_input_reals n)
        val (real, imag) = FftReal.forward (f, input);
    in
        foldl (fn (i, acc) => acc + Math.sqrt (Vector.sub (real, i) *
                                               Vector.sub (real, i) +
                                               Vector.sub (imag, i) *
                                               Vector.sub (imag, i)))
              0.0
              (List.tabulate (n, fn x => x))
    end

fun once_real_inv n f =
    let val real = Array.vector (make_input_reals n)
        val imag = Array.vector (make_input_imags n)
        val time = FftReal.inverse (f, real, imag)
    in
        foldl (fn (i, acc) => acc + abs (Vector.sub (time, i)))
              0.0
              (List.tabulate (n, fn x => x))
    end
        
fun many_times n itr =
    let val f = Fft.fft n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once n f)))
    end
        
fun many_times_real n itr =
    let val f = FftReal.fft_real n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once_real n f)))
    end
        
fun many_times_real_inv n itr =
    let val f = FftReal.fft_real n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once_real_inv n f)))
    end

fun timed_call f =
    let val start = Time.now ()
        val result = f ()
        val finish = Time.now ()
    in (Time.-(finish, start), result)
    end

fun benchmark n =
    app (fn (func, name) =>
            let
	        val itr = 2000
                val (time, result) = timed_call (fn _ => func n itr)
	        val secs = Time.toReal time
            in 
                print ("result = " ^
                       (Real.toString result) ^ " in " ^
                       (Real.toString secs) ^ " sec (" ^
	               (Int.toString (Real.round ((Real.fromInt itr) / secs))) ^ " x " ^
	               (Int.toString n) ^ "-pt itr/sec) [" ^ name ^ "]\n")
            end)
        [ (many_times, "complex"),
          (many_times_real, "real"),
          (many_times_real_inv, "real inverse") ]
        
fun main () =
    app benchmark [ 512, 1024, 2048, 8192 ]

