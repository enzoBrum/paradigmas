all:
	ghc sudoku.hs dlx.hs  -o main -O3
clean:
	rm *.o *.hi main