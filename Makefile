
test:	test.mlb fft.sig fft.sml Makefile
	../sml-buildscripts/polybuild test.mlb && mlton test.mlb

clean:
	rm -f test
