        
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
    { name = "dirac",
      real = true,
      input = ([ 1.0, 0.0, 0.0, 0.0 ], [ 0.0,  0.0, 0.0, 0.0 ]),
      output = ([ 1.0, 1.0, 1.0, 1.0 ], [ 0.0,  0.0, 0.0, 0.0 ]) },
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
    }		   
];      

fun test () =
    app (fn testcase =>
            let
                val real_arr = Array.fromList (#1 (#input testcase))
                val imag_arr = Array.fromList (#2 (#input testcase))
		val real_vec = Array.vector real_arr
		val imag_vec = Array.vector imag_arr
                fun print_vec vec =
                    Vector.app (fn x => print (" " ^ (Real.toString x))) vec
		and wrong a b = Real.abs (a - b) > 1e~12
		and check_vec vec lst scale =
                    Vector.appi
                        (fn (i, x) =>
                            if wrong x (Vector.sub (vec, i) / scale) then
				(print_vec vec;
				 print "\n";
                                 raise Fail ("Failed: " ^ (#name testcase)))
                            else ())
                        (Vector.fromList lst)
		and check (real, imag) selector scale sort =
		    (check_vec real (#1 (selector testcase)) scale;
		     check_vec imag (#2 (selector testcase)) scale;
		     print ("PASS: " ^ sort ^ " " ^ (#name testcase) ^ "\n"))
		and check_arrs (real, imag) =
		    check (Array.vector real, Array.vector imag)
                val n = Array.length real_arr
		val n' = Real.fromInt n
                val f = Fft.fft n
		val f_out as (f_re, f_im) = Fft.forward (f, real_vec, imag_vec)
		val i_out as (i_re, i_im) = Fft.inverse (f, f_re, f_im)
		val fr_out as (fr_re, fr_im) = Fft.forward_real (f, real_vec)
		val ir_re = Fft.inverse_real (f, fr_re, fr_im)
            in
		check f_out #output 1.0 "forward";
		check i_out #input n' "inverse";
		if #real testcase then
		    (check fr_out #output 1.0 "forward_real";
		     check_vec ir_re (#1 (#input testcase)) n';
		     print ("PASS: inverse_real " ^ (#name testcase) ^ "\n"))
		else ();
                Fft.forward_inplace (f, real_arr, imag_arr);
		check_arrs (real_arr, imag_arr) #output 1.0 "forward_inplace";
                Fft.inverse_inplace (f, real_arr, imag_arr);
		check_arrs (real_arr, imag_arr) #input n' "inverse_inplace"
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
        
fun many_times n itr =
    let val f = Fft.fft n in
        List.foldl (op+) 0.0 (List.tabulate (itr, (fn _ => once n f)))
    end

fun timed_call f =
    let val start = Time.now ()
        val result = f ()
        val finish = Time.now ()
    in (Time.-(finish, start), result)
    end

fun benchmark n =
    let
	val itr = 2000
        val (time, result) = timed_call (fn _ => many_times n itr)
	val secs = Time.toReal time
    in 
        print ("result = " ^
               (Real.toString result) ^ " in " ^
               (Real.toString secs) ^ " sec (" ^
	       (Int.toString (Real.round ((Real.fromInt itr) / secs))) ^ " x " ^
	       (Int.toString n) ^ "-pt itr/sec)\n")
    end
        
fun main () =
    let
        val () = test ()
    in
	app benchmark [ 512, 1024, 2048, 8192 ]
    end

val _ = main ();
