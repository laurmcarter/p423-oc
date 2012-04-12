p423-oc: Compiler.hs
	ghc -o $@ $<

clean:
	rm -rf p423-oc *.hi *.o

.PHONY: clean