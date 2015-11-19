                 
structure Fft :> FFT = struct

type t = {
    cos : real vector,
    sin : real vector,
    levels : int
}

fun for count f =
    let val n = ref 0 in
        while !n < count do (f (!n); n := !n + 1)
    end
             
fun power_of_two 1 = 0
  | power_of_two n =
    if n <= 0 then raise Fail "invalid argument"
    else if Int.mod (n, 2) = 1 then raise Fail "power-of-two sizes only please"
    else 1 + power_of_two (Int.quot (n, 2))

fun fft size =
    let
        val phase = fn i => 2.0 * Math.pi * Real.fromInt(i) / Real.fromInt(size)
        val cos = Vector.tabulate (size, Math.cos o phase)
        val sin = Vector.tabulate (size, Math.sin o phase)
        val levels = power_of_two size
    in
        { cos = cos, sin = sin, levels = levels }
    end
        
fun forward_inplace (t : t, real, imag) =

    let
        open Array

        val size = length real

        val _ = if size = Vector.length (#cos t) then () else
                raise Fail "Argument length does not match FFT size"
        val _ = if size = length imag then () else
                raise Fail "Arguments have differing lengths"

        val levels = #levels t
		     
        fun reverse_bits (x, bits) =
            let open Word
                val r = ref 0w0
                val w = ref (fromInt x)
                val b = ref (fromInt bits)
            in
                while !b > 0w0 do (r := orb (<< (!r, 0w1), andb (!w, 0w1));
                                   w := >> (!w, 0w1);
                                   b := !b - 0w1);
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
                                    val c = Vector.sub (#cos t, h * step)
                                    val s = Vector.sub (#sin t, h * step)
                                    val re = sub (real, k) * c + sub (imag, k) * s
                                    val im = sub (imag, k) * c - sub (real, k) * s
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

fun inverse_inplace (t : t, real, imag) =
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
            
end

