
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
                          
