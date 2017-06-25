
all:	test

test:	test.mlb fft.sig fft.sml fft-real.sig fft-real.sml test.sml Makefile
	mlton test.mlb
	./test

timings:	timings.mlb fft.sig fft.sml timings.sml Makefile
	mlton timings.mlb
	./timings

clean:
	rm -f test timings
