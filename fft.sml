(*
    Fast Fourier Transform in Standard ML.
    Supports power-of-two sizes only.

    Written by Chris Cannam, cannam@all-day-breakfast.com.
    Modelled on the Nayuki project multi-language implementation
    (http://www.nayuki.io/page/free-small-fft-in-multiple-languages).

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
                 
structure Fft :>
          FFT
              where type vec = RealVector.vector
              where type arr = RealArray.array = struct

type vec = RealVector.vector
type arr = RealArray.array

type t = {
    cos : vec,
    sin : vec,
    levels : int
}
             
structure VEC = RealVector
             
fun for count f =
    let val n = ref 0 in
        while !n < count do (f (!n); n := !n + 1)
    end
             
fun power_of_two 1 = 0
  | power_of_two n =
    if n <= 0 then raise Fail "invalid argument"
    else if Int.mod (n, 2) = 1 then raise Fail "power-of-two sizes only please"
    else 1 + power_of_two (Int.quot (n, 2))

fun new size =
    let
        val phase = fn i => 2.0 * Math.pi * Real.fromInt(i) / Real.fromInt(size)
        val cos = VEC.tabulate (size, Math.cos o phase)
        val sin = VEC.tabulate (size, Math.sin o phase)
        val levels = power_of_two size
    in
        { cos = cos, sin = sin, levels = levels }
    end

fun size (fft : t) =
    let fun size_for 0 = 1
          | size_for lev = 2 * size_for (lev - 1)
    in size_for (#levels fft)
    end
        
fun forward_inplace (t : t, real, imag) =

    let
        open RealArray

        val size = length real

        val _ = if size = VEC.length (#cos t) then () else
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
                                    val c = VEC.sub (#cos t, h * step)
                                    val s = VEC.sub (#sin t, h * step)
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
    let open RealArray
        val re_a = array (VEC.length real, 0.0)
        val im_a = array (VEC.length imag, 0.0)
    in
        copyVec { src = real, dst = re_a, di = 0 };
        copyVec { src = imag, dst = im_a, di = 0 };
        forward_inplace (t, re_a, im_a);
        (vector re_a, vector im_a)
    end
    
fun inverse (t, real, imag) =
    let open RealArray
        val re_a = array (VEC.length real, 0.0)
        val im_a = array (VEC.length imag, 0.0)
    in
        copyVec { src = real, dst = re_a, di = 0 };
        copyVec { src = imag, dst = im_a, di = 0 };
        inverse_inplace (t, re_a, im_a);
        (vector re_a, vector im_a)
    end
            
end

