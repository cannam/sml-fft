
test:	test.mlb fft.sig fft.sml test.sml Makefile
#	../sml-buildscripts/polybuild test.mlb
	mlton test.mlb
	./test

clean:
	rm -f test
