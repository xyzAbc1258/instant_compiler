all: runtimelib, insc_jvm, insc_llvm

runtimelib:
    java -jar ./lib/jasmin.jar ./lib/Runtime.j -d ./lib

build_all:
    cabal configure
    cabal build

insc_jvm: build_all
    cp ..

insc_llvm: build_all
    cp ..