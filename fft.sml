                 
fun for count f =
    let val n = ref 0 in
        while !n < count do (f (!n); n := !n + 1)
    end

        
structure Fft :> FFT = struct

type t = {
    cos : real vector,
    sin : real vector,
    levels : int
}
             
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
                                    val c = Vector.sub (#cos t, h * step)
                                    val s = Vector.sub (#sin t, h * step)
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

                           
structure FftReal :> FFT_REAL = struct

type t = {
    sub : Fft.t,
    cos_f : real vector,
    sin_f : real vector,
    cos_i : real vector,
    sin_i : real vector
}

fun fft_real size =              
    let
        val hs = Int.div (size, 2)
        val real_substate = Fft.fft hs
                
        (* additional factors for real-complex transforms: *)
        val phase = fn i => ~Math.pi * (Real.fromInt (i+1) /
                                        Real.fromInt hs + 0.5)
        val cos_f = Vector.tabulate (hs, Math.cos o phase)
        val sin_f = Vector.tabulate (hs, Math.sin o phase)
        val cos_i = Vector.tabulate (hs, Math.cos o ~ o phase)
        val sin_i = Vector.tabulate (hs, Math.sin o ~ o phase)
    in
        { sub = real_substate,
          cos_f = cos_f, sin_f = sin_f,
          cos_i = cos_i, sin_i = sin_i }
    end

fun forward (t : t, re_in) =
    let
        open Array
        val sz = Vector.length re_in
        val _ = if sz = 2 * Vector.length (#cos_f t) then () else
                raise Fail "Argument length does not match FFTReal size"
        val hs = Int.quot (sz, 2)
        val hhs = Int.quot (hs, 2)
        val re_a = tabulate (hs, (fn i => Vector.sub (re_in, i * 2)))
        val im_a = tabulate (hs, (fn i => Vector.sub (re_in, i * 2 + 1)))
        val re_out = array (sz, 0.0)
        val im_out = array (sz, 0.0)
    in
        Fft.forward_inplace (#sub t, re_a, im_a);
        update (re_out, 0, sub (re_a, 0) + sub (im_a, 0));
        update (re_out, hs, sub (re_a, 0) - sub (im_a, 0));
        for hhs
            (fn i =>
                let
                    val c = Vector.sub (#cos_f t, i)
                    val s = Vector.sub (#sin_f t, i)
                    val k = i + 1
                    val r0 = sub (re_a, k)
                    val r1 = sub (re_a, hs - k)
                    val i0 = sub (im_a, k)
                    val i1 = sub (im_a, hs - k)
                    val tw_r = (r0 - r1) * c - (i0 + i1) * s
                    val tw_i = (r0 - r1) * s + (i0 + i1) * c
                in
                    update (re_out, k, (r0 + r1 + tw_r) / 2.0);
                    update (re_out, hs - k, (r0 + r1 - tw_r) / 2.0);
                    update (re_out, sz - k, sub (re_out, k));
                    update (re_out, hs + k, sub (re_out, hs - k));
                    update (im_out, k, (i0 - i1 + tw_i) / 2.0);
                    update (im_out, hs - k, (tw_i - i0 + i1) / 2.0);
                    update (im_out, sz - k, 0.0 - sub (im_out, k));
                    update (im_out, hs + k, 0.0 - sub (im_out, hs - k))
                end);
        (vector re_out, vector im_out)
    end

fun inverse (t : t, re_in, im_in) =
    let
        open Array
        val sz = Vector.length re_in
        val _ = if sz = 2 * Vector.length (#cos_i t) then () else
                raise Fail "Argument length does not match FFTReal size"
        val _ = if sz = Vector.length im_in then () else
                raise Fail "Arguments have differing lengths"
        val hs = Int.quot (sz, 2)
        val hhs = Int.quot (hs, 2)
        val re_a = array (hs, 0.0)
        val im_a = array (hs, 0.0)
    in
        update (re_a, 0, Vector.sub (re_in, 0) + Vector.sub (re_in, hs));
        update (im_a, 0, Vector.sub (re_in, 0) - Vector.sub (re_in, hs));
        for hhs 
            (fn i =>
                let
                    val c = Vector.sub (#cos_i t, i)
                    val s = Vector.sub (#sin_i t, i)
                    val k = i + 1
                    val r0 = Vector.sub (re_in, k)
                    val r1 = Vector.sub (re_in, hs - k)
                    val i0 = Vector.sub (im_in, k)
                    val i1 = Vector.sub (im_in, hs - k)
                    val tw_r = (r0 - r1) * c - (i0 - i1) * s
                    val tw_i = (r0 - r1) * s + (i0 - i1) * c
                in
                    update (re_a, k, r0 + r1 + tw_r);
                    update (re_a, hs - k, r0 + r1 - tw_r);
                    update (im_a, k, i0 + i1 + tw_i);
                    update (im_a, hs - k, tw_i - i0 + i1)
                end);
        Fft.inverse_inplace (#sub t, re_a, im_a);
        Vector.tabulate (sz, (fn i =>
                                 let val j = Int.quot (i, 2)
                                     val k = Int.mod (i, 2)
                                 in
                                     if k = 0 then sub (re_a, j)
                                     else sub (im_a, j)
                                 end))
    end        

end
                          
