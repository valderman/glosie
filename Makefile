glosie:
	hastec -O2 glosie.hs
	ghc --make -O2 -o api.cgi api.hs

clean:
	rm Main.jsmod glosie.hi glosie.o api.hi api.o
