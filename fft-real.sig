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

signature FFT_REAL = sig
    type t
    type vec

    (* Initialise a real-complex FFT of a given size (must be a power
       of two, at least 2). *)
    val new : int -> t

    (* Return the size of the given FFT. *)
    val size : t -> int
             
    (* Calculate a forward FFT of real input. Return the result as a
       pair of separate (real, imaginary) vectors. The input vector
       must have the size given by \ref size. The result vectors will
       have the same size as the input vector. The transform is
       unscaled. *)
    val forward : t * vec -> vec * vec
		
    (* Calculate a forward FFT of real input. Return the result as a
       pair of separate (real, imaginary) vectors. The input vector
       must have the size given by \ref size. The conjugate half is
       not returned, so the output vectors have size \ref size/2 +
       1. The transform is unscaled. *)
    val forward_ccs : t * vec -> vec * vec
	
    (* Calculate a forward FFT of real input. Return the result as a
       magnitude vector (discarding phase). The input vector must have
       the size given by \ref size. The result vector will have the
       same size as the input vector. *)
    val forward_magnitude : t * vec -> vec

    (* Calculate an inverse FFT of complex input, returning only the
       real part of the result and discarding the imaginary part. The
       input vectors do not need to supply the (assumed) conjugate
       half, so only need to supply \ref size/2 + 1 elements (they may
       be longer than this, but only the first \ref size/2 + 1
       elements will be used). The result vector has size \ref
       size. The transform is unscaled. *)
    val inverse : t * vec * vec -> vec
end

