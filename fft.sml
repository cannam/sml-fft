
structure Fft :> FFT = struct

type fft_rec = {
    cos : real array,
    sin : real array,
    levels : int
}

type real_fft_rec = {
    sub : fft_rec,
    cos : real array,
    sin : real array
}
                   
type t = {
    complex : fft_rec,
    real : real_fft_rec
}

fun power_of_two 1 = 0
  | power_of_two n =
    if n <= 0 then raise Fail "invalid argument"
    else if Int.mod (n, 2) = 1 then raise Fail "power-of-two sizes only please"
    else 1 + power_of_two (Int.quot (n, 2))

fun fft size =
    let
        open Array
                              
        fun fft_complex size =
            let
                (* factors for complex-complex transforms: *)
                val phase = fn i => 2.0 * Math.pi *
                                    Real.fromInt(i) / Real.fromInt(size)
                val cos = tabulate (size, Math.cos o phase)
                val sin = tabulate (size, Math.sin o phase)
                val levels = power_of_two size
            in
                {
                  cos = cos,
                  sin = sin,
                  levels = levels
                }
            end

        val main = fft_complex size

        val hs = Int.div (size, 2)
        val real_substate = fft_complex hs
                
        (* additional factors for real-complex transforms: *)
        val phase_r = fn i => ~Math.pi * (Real.fromInt (i+1) /
                                          Real.fromInt hs + 0.5)
        val cos_r = tabulate (hs, Math.cos o phase_r)
        val sin_r = tabulate (hs, Math.sin o phase_r)
    in
        {
          complex = main,
          real = {
              sub = real_substate,
              cos = cos_r,
              sin = sin_r
          }
        }
    end
                 
fun for count f =
    let val n = ref 0 in
        while !n < count do (f (!n); n := !n + 1)
    end

fun do_forward_inplace (frec : fft_rec, real, imag) =

    let
        open Array

        val size = length (#cos frec)
        val levels = #levels frec

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
                                    val c = sub (#cos frec, h * step)
                                    val s = sub (#sin frec, h * step)
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

fun forward_inplace (t : t, real, imag) =
    do_forward_inplace (#complex t, real, imag)
        
fun inverse_inplace (t : t, real, imag) =
    do_forward_inplace (#complex t, imag, real)

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

fun forward_real (t : t, real) =
    let
        open Array
        val sz = Vector.length real
        val hs = Int.quot (sz, 2)
        val hhs = Int.quot (hs, 2)
        val re_a = tabulate (hs, (fn i => Vector.sub (real, i * 2)))
        val im_a = tabulate (hs, (fn i => Vector.sub (real, i * 2 + 1)))
        val re_out = array (Vector.length real, 0.0)
        val im_out = array (Vector.length real, 0.0)
        val real_rec = #real t
    in
        do_forward_inplace (#sub real_rec, re_a, im_a);
        update (re_out, 0, sub (re_a, 0) + sub (im_a, 0));
        update (re_out, hs, sub (re_a, 0) - sub (im_a, 0));
        for hhs
            (fn i =>
                let
                    val c = sub (#cos real_rec, i)
                    val s = sub (#sin real_rec, i)
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
                          
