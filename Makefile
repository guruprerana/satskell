all:
	ghc MySat.hs -O2

clean:
	rm *.hi *.o Solver/*.o Solver/*.hi CNF/*.o CNF/*.hi