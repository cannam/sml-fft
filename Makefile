
all:	test

test:	test.mlb fft.sig fft.sml test.sml Makefile
	mlton test.mlb
	./test

timings:	timings.mlb fft.sig fft.sml test.sml Makefile
	mlton timings.mlb
	./timings

clean:
	rm -f test timings
