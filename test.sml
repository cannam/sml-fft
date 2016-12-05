        
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
                fun good expected obtained =
                    if Real.abs (obtained - expected) < 1e~11 then true
                    else (print ("Expected " ^ (Real.toString expected) ^
                                 ", obtained " ^ (Real.toString obtained) ^
                                 ", diff " ^ (Real.toString (obtained-expected))
                                 ^ "\n");
                          false)
                fun pass sort =
                    print ("PASS: " ^ sort ^ " " ^ name ^ "\n")
                fun fail vec sort =
                    (print ("FAIL: " ^ sort ^ " " ^ name ^ "\n");
                     print_vec vec;
                     print "\n";
                     raise Fail ("Failed: " ^ name))
                fun check_vec vec lst scale sort =
                    let val factor =
                            if scale then 1.0 / Real.fromInt (Vector.length vec)
                            else 1.0
                    in
                        Vector.appi
                            (fn (i, x) =>
                                let val y = (Vector.sub (vec, i) * factor)
                                in
                                    if good x y then ()
                                    else fail vec sort
                                end)
                            (Vector.fromList lst)
                    end
                fun check (real, imag) selector scale sort =
                    let val (tr, ti) = selector testcase
                    in
                        check_vec real tr scale sort;
                        check_vec imag ti scale sort;
                        pass sort
                    end
                fun check_arrs (real, imag) =
                    check (Array.vector real, Array.vector imag)

                val f = Fft.new (Vector.length real_vec)
                val fr = FftReal.new (Vector.length real_vec)
                val f_out as (f_re, f_im) = Fft.forward (f, real_vec, imag_vec)
                val i_out as (i_re, i_im) = Fft.inverse (f, f_re, f_im)
                val fr_out as (fr_re, fr_im) = FftReal.forward (fr, real_vec)
                val ir_re = FftReal.inverse (fr, fr_re, fr_im)
            in
                if Fft.size f <> Vector.length real_vec
                then raise Fail "FFT size does not match vector size"
                else ();

                if FftReal.size fr <> Vector.length real_vec
                then raise Fail "Real FFT size does not match vector size"
                else ();
                
                check f_out #output false "forward";
                check i_out #input true "inverse";

                if #real testcase then
                    (check fr_out #output false "forward_real";
                     check_vec ir_re (#1 (#input testcase)) true "inverse_real";
                     pass "inverse_real")
                else ();

                Fft.forward_inplace (f, real_arr, imag_arr);
                check_arrs (real_arr, imag_arr) #output false "forward_inplace";

                Fft.inverse_inplace (f, real_arr, imag_arr);
                check_arrs (real_arr, imag_arr) #input true "inverse_inplace"

            end)
        testcases

fun main () = test ()
