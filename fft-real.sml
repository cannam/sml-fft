(*
    Fast Fourier Transform in Standard ML.
    Supports power-of-two sizes only.

    Written by Chris Cannam, cannam@all-day-breakfast.com.
    Real-complex wrapper adapted from KissFFT by Mark Borgerding.

    Copyright 2015 Particular Programs Ltd.

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of Chris Cannam and
    Particular Programs Ltd shall not be used in advertising or
    otherwise to promote the sale, use or other dealings in this
    Software without prior written authorization.
*)
                                  
structure FftReal :>
          FFT_REAL
              where type vec = RealVector.vector = struct

type vec = RealVector.vector
                               
type t = {
    sub : Fft.t,
    cos_f : vec,
    sin_f : vec,
    cos_i : vec,
    sin_i : vec
}

structure VEC = RealVector
             
fun for count f =
    let val n = ref 0 in
        while !n < count do (f (!n); n := !n + 1)
    end
       
fun new size =              
    let
        val hs = Int.div (size, 2)
        val real_substate = Fft.new hs
                
        (* additional factors for real-complex transforms: *)
        val phase = fn i => ~Math.pi * (Real.fromInt (i+1) /
                                        Real.fromInt hs + 0.5)
        val cos_f = VEC.tabulate (hs, Math.cos o phase)
        val sin_f = VEC.tabulate (hs, Math.sin o phase)
        val cos_i = VEC.tabulate (hs, Math.cos o ~ o phase)
        val sin_i = VEC.tabulate (hs, Math.sin o ~ o phase)
    in
        { sub = real_substate,
          cos_f = cos_f, sin_f = sin_f,
          cos_i = cos_i, sin_i = sin_i }
    end

fun size (t : t) =
    2 * Fft.size (#sub t)

fun forward_inplace (t : t, re_in, re_out, im_out) =
    let
        open RealArray
        val sz = VEC.length re_in
        val _ = if sz = size t then () else
                raise Fail ("Argument length " ^ (Int.toString sz) ^
                            " does not match (forward) FFTReal size " ^
                            (Int.toString (size t)))
        val hs = Int.quot (sz, 2)
        val hhs = Int.quot (hs, 2)
        val re_a = tabulate (hs, (fn i => VEC.sub (re_in, i * 2)))
        val im_a = tabulate (hs, (fn i => VEC.sub (re_in, i * 2 + 1)))
    in
        Fft.forward_inplace (#sub t, re_a, im_a);
        update (re_out, 0, sub (re_a, 0) + sub (im_a, 0));
        update (re_out, hs, sub (re_a, 0) - sub (im_a, 0));
        for hhs
            (fn i =>
                let
                    val c = VEC.sub (#cos_f t, i)
                    val s = ~ VEC.sub (#sin_f t, i)
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
                    update (im_out, k, (i0 - i1 + tw_i) / 2.0);
                    update (im_out, hs - k, (tw_i - i0 + i1) / 2.0)
                end)
    end
                 
fun forward_ccs (t : t, re_in) =
    let
        open RealArray
        val sz = VEC.length re_in
        val hs = Int.quot (sz, 2)
        val re_out = array (hs + 1, 0.0)
        val im_out = array (hs + 1, 0.0)
    in
        forward_inplace (t, re_in, re_out, im_out);
        (vector re_out, vector im_out)
    end

fun forward (t : t, re_in) =
    let
        open RealArray
        val sz = VEC.length re_in
        val hs = Int.quot (sz, 2)
        val re_out = array (sz, 0.0)
        val im_out = array (sz, 0.0)
    in
        forward_inplace (t, re_in, re_out, im_out);
        for (hs + 1)
            (fn i =>
                let
                    val re = sub (re_out, i)
                    val im = sub (im_out, i)
                in
                    update (re_out, i, re);
                    update (im_out, i, im);
                    if i > 0
                    then 
                        (update (re_out, sz - i, re);
                         update (im_out, sz - i, 0.0 - im))
                    else ()
                end);
        (vector re_out, vector im_out)
    end

fun forward_magnitude (t : t, re_in) =
    let val (re, im) = forward (t, re_in)
        open RealVector
        val sz = length re_in
    in
        tabulate (sz, fn i =>
                         Math.sqrt (sub (re, i) * sub (re, i) +
                                    sub (im, i) * sub (im, i)))
    end

fun inverse (t : t, re_in, im_in) =
    let
        open RealArray
        val insz = VEC.length re_in
        val sz = size t
        val hs = Int.quot (sz, 2)
        val _ = if insz >= hs + 1 then () else
                raise Fail ("Argument length " ^ (Int.toString insz) ^
                            " too short for (inverse, non-conjugate) FFTReal input size " ^
                            (Int.toString (hs + 1)))
        val _ = if insz = VEC.length im_in then () else
                raise Fail "Arguments to inverse have differing lengths"
        val hhs = Int.quot (hs, 2)
        val re_a = array (hs, 0.0)
        val im_a = array (hs, 0.0)
    in
        update (re_a, 0, VEC.sub (re_in, 0) + VEC.sub (re_in, hs));
        update (im_a, 0, VEC.sub (re_in, 0) - VEC.sub (re_in, hs));
        for hhs 
            (fn i =>
                let
                    val c = VEC.sub (#cos_i t, i)
                    val s = VEC.sub (#sin_i t, i)
                    val k = i + 1
                    val r0 = VEC.sub (re_in, k)
                    val r1 = VEC.sub (re_in, hs - k)
                    val i0 = VEC.sub (im_in, k)
                    val i1 = ~(VEC.sub (im_in, hs - k))
                    val tw_r = (r0 - r1) * c - (i0 - i1) * s
                    val tw_i = (r0 - r1) * s + (i0 - i1) * c
                in
                    update (re_a, k, r0 + r1 + tw_r);
                    update (im_a, k, i0 + i1 + tw_i);
                    update (re_a, hs - k, r0 + r1 - tw_r);
                    update (im_a, hs - k, tw_i - i0 - i1)
                end);
        Fft.inverse_inplace (#sub t, re_a, im_a);
        VEC.tabulate (sz, fn i =>
                             let val j = Int.quot (i, 2)
                                 val k = Int.mod (i, 2)
                             in
                                 if k = 0 then sub (re_a, j)
                                 else sub (im_a, j)
                             end)
    end        

end
                          
