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
       
signature FFT = sig
    type t

    (* Initialise an FFT of a given size (must be a power of two). *)
    val fft : int -> t

    (* Calculate a forward FFT of complex input, expressed as a pair
       of separate (real, imaginary) vectors. Return the result in the
       same form. Result vectors will have the same size as the input
       vectors. *)
    val forward : t * real vector * real vector -> real vector * real vector
							     
    (* Calculate a forward FFT of complex input, expressed as a pair
       of separate mutable (real, imaginary) arrays. Return the result
       in the same arrays, replacing the inputs. *)
    val forward_inplace : t * real array * real array -> unit

    (* Calculate an inverse FFT of complex input, expressed as a pair
       of separate (real, imaginary) vectors. Return the result in the
       same form. Result vectors will have the same size as the input
       vectors. The forward and inverse transforms are both unscaled, 
       so this is not a true inverse. *)
    val inverse : t * real vector * real vector -> real vector * real vector

    (* Calculate an inverse FFT of complex input, expressed as a pair
       of separate mutable (real, imaginary) arrays. Return the result
       in the same arrays, replacing the inputs. *)
    val inverse_inplace : t * real array * real array -> unit
end
                         
