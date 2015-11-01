all:
	cd parser && $(MAKE)
	ghc --make src/Compiler.hs -i./parser -i./src -DJVM -XScopedTypeVariables -o insc_jvm
	ghc --make src/Compiler.hs -i./parser -i./src -DLLVM -XScopedTypeVariables -o insc_llvm
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi insc_jvm
	cd src && rm -f *.hi *.o
	cd parser && $(MAKE) clean
	-rm -f parser/TestInstant
	rm -f insc_jvm insc_llvm
