        
(* Unit tests *)

val testcases = [
    { name = "dc",
      real = true,
      input = ([ 1.0, 1.0, 1.0, 1.0 ], [ 0.0, 0.0, 0.0, 0.0 ]),
      output = ([ 4.0, 0.0, 0.0, 0.0 ], [ 0.0, 0.0, 0.0, 0.0 ]) },
    { name = "c_dc",
      real = false,
      input = ([ 1.0, 1.0, 1.0, 1.0 ], [ 1.0, 1.0, 1.0, 1.0 ]),
      output = ([ 4.0, 0.0, 0.0, 0.0 ], [ 4.0, 0.0, 0.0, 0.0 ]) },
    { name = "dirac",
      real = true,
      input = ([ 1.0, 0.0, 0.0, 0.0 ], [ 0.0,  0.0, 0.0, 0.0 ]),
      output = ([ 1.0, 1.0, 1.0, 1.0 ], [ 0.0,  0.0, 0.0, 0.0 ]) },
    { name = "sine",
      real = true,
      input = ([ 0.0, 1.0, 0.0, ~1.0 ], [ 0.0,  0.0, 0.0, 0.0 ]),
      output = ([ 0.0, 0.0, 0.0,  0.0 ], [ 0.0, ~2.0, 0.0, 2.0 ]) },
    { name = "cosine",
      real = true,
      input = ([ 1.0, 0.0, ~1.0, 0.0 ], [ 0.0, 0.0, 0.0, 0.0 ]),
      output = ([ 0.0, 2.0,  0.0, 2.0 ], [ 0.0, 0.0, 0.0, 0.0 ]) },
    { name = "sine_cosine",
      real = true,
      input = ([ 0.5, 1.0, ~0.5, ~1.0 ], [ 0.0,  0.0, 0.0, 0.0 ]),
      output = ([ 0.0, 1.0,  0.0,  1.0 ], [ 0.0, ~2.0, 0.0, 2.0 ]) },
    { name = "nyquist",
      real = true,
      input = ([ 1.0, ~1.0, 1.0, ~1.0 ], [ 0.0,  0.0, 0.0, 0.0 ]),
      output = ([ 0.0,  0.0, 4.0,  0.0 ], [ 0.0,  0.0, 0.0, 0.0 ]) },
    { name = "random",
      real = false,
      input = ([ ~0.24392125308057722, 0.03443898163344272, 0.3448145656738877,
		 ~0.9625837464603908, 3.366568317669671, 0.9947191221586653,
		 ~1.5038984435999945, 1.3859898682581235, ~1.1230576306688778,
		 ~1.6757487116512024, ~1.5874436867863229, ~2.0794018781307155,
		 ~0.5450152775818973, 0.7530907176983748, 1.0743170685904255,
		 3.1787609811018775 ],
	       [ ~4.6035204166398245, 2.0317875269650436, ~0.9395023884462228,
		 3.472863250686892, ~4.39411546028619, ~4.029033340645395,
		 2.2645504926014395, ~3.3231227640063543, ~4.379656709017565,
		 ~4.424101879536996, 0.7176597582788755, 3.5184499663505555,
		 4.213506718965959, ~2.28304306731049, 3.493978453385939,
		 ~0.06204797310545107 ]),
      output = ([ 1.4116289948244898, ~3.441592791035387, 2.9223745121794504,
		  8.4254296203132757, ~9.183748587993156, ~5.7714687057974405,
		  8.9185066378501752, 10.713441340537777, ~1.8469016743918605,
		  ~9.7188605206474428, ~23.260181512163356, 0.087059443618590748,
		  15.437317892913802, ~11.982021189175127, ~5.3348273332151832,
		  18.721103822892161 ],
		[ ~8.7253478317597821, 4.4234426781065554, ~4.779868103546562,
		  7.1906396712405298, ~13.284207067868035, ~5.8411553712644615,
		  2.725220081952985, ~7.9229528687760915, 1.4711487294446055,
		  ~12.385723253278199, ~11.199173172922016, 1.7032291082897322,
		  ~16.116737297727266, ~2.7383532650592044, ~21.956452342833046,
		  13.779963639763066 ])
    },
    { name = "random_real",
      real = true,
      input = ([ ~0.24392125308057722, 0.03443898163344272, 0.3448145656738877,
		 ~0.9625837464603908, 3.366568317669671, 0.9947191221586653,
		 ~1.5038984435999945, 1.3859898682581235, ~1.1230576306688778,
		 ~1.6757487116512024, ~1.5874436867863229, ~2.0794018781307155,
		 ~0.5450152775818973, 0.7530907176983748, 1.0743170685904255,
		 3.1787609811018775 ],
	       [ 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0, 
                 0.0, 0.0, 0.0,
                 0.0 ]),
      output = ([ 1.41162899482, 7.63975551593, ~1.20622641052, ~1.77829578443,
                  3.12678465246, ~2.84220463109, ~7.17083743716, 0.497290409945,
                  ~1.84690167439, 0.497290409945, ~7.17083743716, ~2.84220463109,
                  3.12678465246, ~1.77829578443, ~1.20622641052, 7.63975551593 ],
                [ 0.0, ~4.67826048083, 8.58829211964, 4.96449646815,
                  1.41626511493, ~3.77219223978, 6.96219662744, 2.23138519225,
                  0.0, ~2.23138519225, ~6.96219662744, 3.77219223978,
                  ~1.41626511493, ~4.96449646815, ~8.58829211964, 4.67826048083 ])
    }		   
];      

fun test () =
    app (fn testcase =>
            let
		val name = #name testcase
                val real_arr = Array.fromList (#1 (#input testcase))
                val imag_arr = Array.fromList (#2 (#input testcase))
		val real_vec = Array.vector real_arr
		val imag_vec = Array.vector imag_arr

                fun print_vec vec =
                    print (Vector.foldr
                               (fn (x, s) => if s = "" then Real.toString x
                                             else (Real.toString x) ^ ", " ^ s)
                               "" vec)
		and good expected obtained =
                    if Real.abs (obtained - expected) < 1e~11 then true
                    else (print ("Expected " ^ (Real.toString expected) ^
                                 ", obtained " ^ (Real.toString obtained) ^
                                 ", diff " ^ (Real.toString (obtained-expected))
                                 ^ "\n");
                          false)
		and pass sort =
		    print ("PASS: " ^ sort ^ " " ^ name ^ "\n")
		and fail vec sort =
		    (print ("FAIL: " ^ sort ^ " " ^ name ^ "\n");
		     print_vec vec;
		     print "\n";
		     raise Fail ("Failed: " ^ name))
		and check_vec vec lst scale sort =
		    let val factor =
			    if scale then 1.0 / Real.fromInt (Vector.length vec)
			    else 1.0
		    in
			Vector.appi
                            (fn (i, x) =>
				if good x (Vector.sub (vec, i) * factor) then ()
				else fail vec sort) 
                            (Vector.fromList lst)
		    end
		and check (real, imag) selector scale sort =
		    (check_vec real (#1 (selector testcase)) scale sort;
		     check_vec imag (#2 (selector testcase)) scale sort;
		     pass sort)
		and check_arrs (real, imag) =
		    check (Array.vector real, Array.vector imag)

                val f = Fft.fft (Vector.length real_vec)
                val fr = FftReal.fft_real (Vector.length real_vec)
		val f_out as (f_re, f_im) = Fft.forward (f, real_vec, imag_vec)
		val i_out as (i_re, i_im) = Fft.inverse (f, f_re, f_im)
		val fr_out as (fr_re, fr_im) = FftReal.forward (fr, real_vec)
		val ir_re = FftReal.inverse (fr, fr_re, fr_im)
            in
		check f_out #output false "forward";
		check i_out #input true "inverse";

		if #real testcase then
		    (check fr_out #output false "forward_real";
		     check_vec ir_re (#1 (#input testcase)) true;
		     pass "inverse_real")
		else ();

                Fft.forward_inplace (f, real_arr, imag_arr);
		check_arrs (real_arr, imag_arr) #output false "forward_inplace";

                Fft.inverse_inplace (f, real_arr, imag_arr);
		check_arrs (real_arr, imag_arr) #input true "inverse_inplace"

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

fun once_real n f =
    let val input = Array.vector (make_input_reals n)
        val (real, imag) = FftReal.forward (f, input);
    in
        foldl (fn (i, acc) => acc + Math.sqrt (Vector.sub (real, i) *
                                               Vector.sub (real, i) +
                                               Vector.sub (imag, i) *
                                               Vector.sub (imag, i)))
              0.0
              (List.tabulate (n, fn x => x))
    end

fun once_real_inv n f =
    let val real = Array.vector (make_input_reals n)
        val imag = Array.vector (make_input_imags n)
        val time = FftReal.inverse (f, real, imag)
    in
        foldl (fn (i, acc) => acc + abs (Vector.sub (time, i)))
              0.0
              (List.tabulate (n, fn x => x))
    end
        
fun many_times n itr =
    let val f = Fft.fft n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once n f)))
    end
        
fun many_times_real n itr =
    let val f = FftReal.fft_real n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once_real n f)))
    end
        
fun many_times_real_inv n itr =
    let val f = FftReal.fft_real n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once_real_inv n f)))
    end

fun timed_call f =
    let val start = Time.now ()
        val result = f ()
        val finish = Time.now ()
    in (Time.-(finish, start), result)
    end

fun benchmark n =
    app (fn (func, name) =>
            let
	        val itr = 2000
                val (time, result) = timed_call (fn _ => func n itr)
	        val secs = Time.toReal time
            in 
                print ("result = " ^
                       (Real.toString result) ^ " in " ^
                       (Real.toString secs) ^ " sec (" ^
	               (Int.toString (Real.round ((Real.fromInt itr) / secs))) ^ " x " ^
	               (Int.toString n) ^ "-pt itr/sec) [" ^ name ^ "]\n")
            end)
        [ (many_times, "complex"),
          (many_times_real, "real"),
          (many_times_real_inv, "real inverse") ]
        
fun main () =
    let
        val () = test ()
    in
	app benchmark [ 512, 1024, 2048, 8192 ]
    end

