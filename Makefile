p423-oc: Compiler.hs
	ghc -o $@ $<
	cp $@ ..

clean:
	rm -rf p423-oc *.hi *.o

.PHONY: clean