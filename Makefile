all: runtimelib
	cabal v2-configure
	cabal v2-build
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.6.4/instant-compiler-0.1.0.0/x/insc_jvm/build/insc_jvm/insc_jvm .
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.6.4/instant-compiler-0.1.0.0/x/insc_llvm/build/insc_llvm/insc_llvm .

runtimelib: ./lib/jasmin.jar ./lib/Runtime.j
	java -jar ./lib/jasmin.jar ./lib/Runtime.j -d ./lib

