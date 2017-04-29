interp: Main.hs
	ghc -o $@ $^

clean:
	rm -f *.o *.hi interp