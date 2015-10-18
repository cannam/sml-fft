
signature FFT = sig
    type t

    val fft : int -> t
             
    val forward : t * real vector * real vector -> real vector * real vector
    val forward_real : t * real vector -> real vector * real vector
    val forward_inplace : t * real array * real array -> unit

    val inverse : t * real vector * real vector -> real vector * real vector
    val inverse_real : t * real vector * real vector -> real vector
    val inverse_inplace : t * real array * real array -> unit
end

structure Fft : FFT = struct

type t = { cos : real array, sin : real array, levels : int }

fun fft size =
    let
        open Array
        val factor = 2.0 * Math.pi / Real.fromInt(size)
        val cos = tabulate (size, fn i => Math.cos (factor * Real.fromInt(i)))
        val sin = tabulate (size, fn i => Math.sin (factor * Real.fromInt(i)))
        val levels =
            let val log = Math.log10 (Real.fromInt size) / Math.log10 2.0 in
                if Real.round (Math.pow(2.0, log)) <> size then
                    raise Fail "power-of-two sizes only please"
                else
                    Real.round log
            end
    in
        { cos = cos, sin = sin, levels = levels }
    end

fun forward_inplace (t : t, real, imag) =
    let
        open Array

        val size = length (#cos t)
        val levels = #levels t
                 
        fun for count f =
            let val n = ref 0 in
                while !n < count do (f (!n); n := !n + 1)
            end

        fun reverse_bits (x, bits) =
            let open Word
                val w = ref (fromInt x)
                val r = ref (fromInt 0)
                val b = ref (fromInt bits)
                val one = fromInt 1
                val zero = fromInt 0
            in
                while !b > zero do (r := orb (<< (!r, one), andb (!w, one));
                                    w := >> (!w, one);
                                    b := !b - one);
                toInt (!r)
            end

        fun exchange (arr, i, j) =
            let val temp = sub (arr, i) in
                update (arr, i, sub (arr, j));
                update (arr, j, temp)
            end
                
        fun permute () =
            for size (fn i =>
                         let val j = reverse_bits (i, levels) in
                             if j > i then
                                 (exchange (real, i, j);
                                  exchange (imag, i, j))
                             else ()
                         end)
                
        fun do_scale scale =
            let val half = Int.quot (scale, 2)
                val step = Int.quot (size, scale)
            in
                for step 
                    (fn i' =>
                        for half
                            (fn h =>
                                let val i = i' * scale
                                    val j = h + i
                                    val k = j + half
                                    val c = sub (#cos t, h * step)
                                    val s = sub (#sin t, h * step)
                                    val re = sub (real, k) * c + sub (imag, k) * s;
                                    val im = sub (imag, k) * c - sub (real, k) * s;
                                in
                                    update (real, k, sub (real, j) - re);
                                    update (imag, k, sub (imag, j) - im);
                                    update (real, j, sub (real, j) + re);
                                    update (imag, j, sub (imag, j) + im)
                                end))
            end

        val scale = ref 2
    in
        permute ();
        for levels (fn _ => (do_scale (!scale); scale := !scale * 2))
    end

fun inverse_inplace (t, real, imag) =
    forward_inplace (t, imag, real)

fun forward (t, real, imag) =
    let open Array
        val re_a = array (Vector.length real, 0.0)
        val im_a = array (Vector.length imag, 0.0)
    in
        copyVec { src = real, dst = re_a, di = 0 };
        copyVec { src = imag, dst = im_a, di = 0 };
        forward_inplace (t, re_a, im_a);
        (vector re_a, vector im_a)
    end

fun forward_real (t, real) =
    let open Array
        val re_a = array (Vector.length real, 0.0)
        val im_a = array (Vector.length real, 0.0)
    in
        copyVec { src = real, dst = re_a, di = 0 };
        forward_inplace (t, re_a, im_a);
        (vector re_a, vector im_a)
    end

fun inverse (t, real, imag) =
    let open Array
        val re_a = array (Vector.length real, 0.0)
        val im_a = array (Vector.length imag, 0.0)
    in
        copyVec { src = real, dst = re_a, di = 0 };
        copyVec { src = imag, dst = im_a, di = 0 };
        inverse_inplace (t, re_a, im_a);
        (vector re_a, vector im_a)
    end

fun inverse_real (t, real, imag) =
    let val (re_out, _) = inverse (t, real, imag) in
        re_out
    end
            
end
                          
        
(* Unit tests *)

val testcases = [
    { name = "dc",
      re_in  = [ 1.0, 1.0, 1.0, 1.0 ], im_in  = [ 0.0, 0.0, 0.0, 0.0 ],
      re_out = [ 4.0, 0.0, 0.0, 0.0 ], im_out = [ 0.0, 0.0, 0.0, 0.0 ] },
    { name = "c_dc",
      re_in  = [ 1.0, 1.0, 1.0, 1.0 ], im_in  = [ 1.0, 1.0, 1.0, 1.0 ],
      re_out = [ 4.0, 0.0, 0.0, 0.0 ], im_out = [ 4.0, 0.0, 0.0, 0.0 ] },
    { name = "sine",
      re_in  = [ 0.0, 1.0, 0.0, ~1.0 ], im_in  = [ 0.0,  0.0, 0.0, 0.0 ],
      re_out = [ 0.0, 0.0, 0.0,  0.0 ], im_out = [ 0.0, ~2.0, 0.0, 2.0 ] },
    { name = "cosine",
      re_in  = [ 1.0, 0.0, ~1.0, 0.0 ], im_in  = [ 0.0, 0.0, 0.0, 0.0 ],
      re_out = [ 0.0, 2.0,  0.0, 2.0 ], im_out = [ 0.0, 0.0, 0.0, 0.0 ] },
    { name = "sine_cosine",
      re_in  = [ 0.5, 1.0, ~0.5, ~1.0 ], im_in  = [ 0.0,  0.0, 0.0, 0.0 ],
      re_out = [ 0.0, 1.0,  0.0,  1.0 ], im_out = [ 0.0, ~2.0, 0.0, 2.0 ] },
    { name = "nyquist",
      re_in  = [ 1.0, ~1.0, 1.0, ~1.0 ], im_in  = [ 0.0,  0.0, 0.0, 0.0 ],
      re_out = [ 0.0,  0.0, 4.0,  0.0 ], im_out = [ 0.0,  0.0, 0.0, 0.0 ] },
    { name = "dirac",
      re_in  = [ 1.0, 0.0, 0.0, 0.0 ], im_in  = [ 0.0,  0.0, 0.0, 0.0 ],
      re_out = [ 1.0, 1.0, 1.0, 1.0 ], im_out = [ 0.0,  0.0, 0.0, 0.0 ] }
];      

fun test () =
    app (fn testcase =>
            let
                open Array
                val real = fromList (#re_in testcase)
                val imag = fromList (#im_in testcase)
                fun print_arr arr =
                    app (fn x => print (" " ^ (Real.toString x))) arr
                fun check arr lst scale =
                    appi
                        (fn (i, x) =>
                            if Real.abs (x - (sub (arr, i) / Real.fromInt scale))
                               > 1e~12 then
                                raise Fail ("Failed: " ^ (#name testcase))
                            else ())
                        (fromList lst)
                val n = length real
                val f = Fft.fft n
            in
                Fft.forward_inplace (f, real, imag);
                print_arr real;
                check real (#re_out testcase) 1;
                print_arr imag;
                check imag (#im_out testcase) 1;

                print ("\nSucceeded (forward): " ^ (#name testcase) ^ "\n");

                Fft.inverse_inplace (f, real, imag);
                print_arr real;
                check real (#re_in testcase) n;
                print_arr imag;
                check imag (#im_in testcase) n;

                print ("\nSucceeded (inverse): " ^ (#name testcase) ^ "\n")
            end)
        testcases

        
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
        
fun many_times n itr =
    let val f = Fft.fft n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once n f)))
    end

fun benchmark f =
    let val start = Time.now ()
        val result = f ()
        val finish = Time.now ()
    in (Time.-(finish, start), result)
    end
               
fun main () =
    let
        val () = test ()
        val n = 2000
        val size = 2048
        val (time, result) = benchmark (fn _ => many_times size n)
        val sec = Time.toReal time
    in 
        print ("result = " ^
               (Real.toString result) ^ " in " ^
               (Real.toString sec) ^ " sec\n" ^
               "that's " ^ (Real.toString (Real.fromInt n / sec)) ^ " per second\n")
    end

val _ = main ();

