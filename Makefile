all:
	cd parser && $(MAKE)
	ghc --make src/CompilerJVM.hs -i./parser -XScopedTypeVariables -o insc_jvm
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi insc_jvm
	cd src && rm -f *.hi *.o
	cd parser && $(MAKE) clean
	-rm -f parser/TestInstant
