
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

signature FFT_REAL = sig
    type t

    (* Initialise a real-complex FFT of a given size (must be a power
       of two, at least 2). *)
    val fft_real : int -> t
             
    (* Calculate a forward FFT of real input. Return the result as a
       pair of separate (real, imaginary) vectors. Result vectors will
       have the same size as the input vector. *)
    val forward : t * real vector -> real vector * real vector
								      
    (* Calculate an inverse FFT of complex input, returning only the
       real part of the result and discarding the imaginary
       part. Result vector will have the same size as the input
       vectors. The forward and inverse transforms are both unscaled,
       so this is not a true inverse. *)
    val inverse : t * real vector * real vector -> real vector
end
                         
