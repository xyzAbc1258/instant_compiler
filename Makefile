all: runtimelib, insc_jvm, insc_llvm

runtimelib:
    java -jar ./lib/jasmin.jar ./lib/Runtime.j -d ./lib

build_all:
    cabal v2-configure
    cabal v2-build

insc_jvm: build_all
    cabal v2-install exe:insc_jvm --install-method=copy --installdir=.

insc_llvm: build_all
    cabal v2-install exe:insc_llvm --install-method=copy --installdir=.